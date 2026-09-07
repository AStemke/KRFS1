# 04_01_03_GKZ_Near_Care_Heatmap.R
#
# Stellt den Anteil wohnortnah behandelter Tumoren nach Diagnose-GKZ dar.
# Der Gebietsvergleich wird unabhaengig von vorhandenen Geometrien erzeugt;
# nur die Baden-Wuerttemberg-Karte benoetigt ein sf-Objekt.


# Hilfsfunktionen -----------------------------
## Maskierte Anteile --------------------------

mask_share_after_threshold <- function(tab) {
  required_cols <- c(
    "n_tumors",
    "n_near",
    "n_not_near",
    "share_near"
  )

  if (!all(required_cols %in% names(tab))) {
    return(tab)
  }

  tab |>
    dplyr::mutate(
      share_near = dplyr::if_else(
        is.na(n_tumors) |
          is.na(n_near) |
          is.na(n_not_near),
        NA_real_,
        as.numeric(share_near)
      )
    )
}


# Konfiguration -----------------------------
## Kartenparameter --------------------------

heatmap_config <- config$geodata$heatmap
map_gkz_digits <- as.integer(
  heatmap_config$gkz_digits
)

map_level_label <- dplyr::case_when(
  map_gkz_digits == 5L ~ "Kreis",
  map_gkz_digits == 8L ~ "Gemeinde",
  TRUE ~ paste0(
    "GKZ-",
    map_gkz_digits,
    "-Steller"
  )
)

map_level_plural <- dplyr::case_when(
  map_gkz_digits == 5L ~ "Kreise",
  map_gkz_digits == 8L ~ "Gemeinden",
  TRUE ~ "Gebiete"
)

map_linewidth <- unname(
  heatmap_config$line_width_by_digits[
    as.character(map_gkz_digits)
  ]
)


# Auswertung -----------------------------
## Zielkohorten --------------------------

if (
  config$analysis$active_output_group %in%
    heatmap_config$output_groups
) {
  stopifnot(
    exists("tumor_base"),
    "gkz_diagnose" %in% names(tumor_base),
    "distance_km" %in% names(tumor_base),
    "near_care" %in% names(tumor_base)
  )


  # Tumordaten -----------------------------
  ## Diagnose-GKZ --------------------------

  tumor_heatmap_base <- tumor_base |>
    dplyr::mutate(
      gkz_join = kr_normalize_gkz(
        gkz_diagnose
      )
    ) |>
    dplyr::filter(
      !is.na(gkz_join),
      gkz_join != "00000000",
      !is.na(distance_km),
      !is.na(near_care)
    )

  gkz_near_care_tab <- tumor_heatmap_base |>
    dplyr::group_by(
      gkz_join
    ) |>
    dplyr::summarise(
      n_tumors = dplyr::n(),
      n_near = sum(
        near_care,
        na.rm = TRUE
      ),
      n_not_near = sum(
        !near_care,
        na.rm = TRUE
      ),
      share_near = safe_divide(
        n_near,
        n_tumors
      ),
      .groups = "drop"
    )

  # Ergebnisse auf achtstelliger GKZ-Ebene speichern.
  ctx <- tc_write_table(
    tab = gkz_near_care_tab,
    table_id = "04_03_gkz_near_care_share",
    ctx = ctx,
    output_category = "map_area",
    threshold = TRUE,
    threshold_n_col = c(
      "n_tumors",
      "n_near",
      "n_not_near"
    ),
    threshold_label_cols = "gkz_join"
  )


  # Gebietsvergleich -----------------------------
  ## Baden-Wuerttemberg und ausserhalb --------------------------

  inside_bw_summary <- gkz_near_care_tab |>
    dplyr::filter(
      substr(
        gkz_join,
        start = 1L,
        stop = 2L
      ) == "08"
    ) |>
    dplyr::summarise(
      area_group = "Baden-Wuerttemberg",
      n_tumors = sum(
        n_tumors,
        na.rm = TRUE
      ),
      n_near = sum(
        n_near,
        na.rm = TRUE
      ),
      n_not_near = sum(
        n_not_near,
        na.rm = TRUE
      ),
      share_near = safe_divide(
        n_near,
        n_tumors
      )
    )

  outside_bw_summary <- gkz_near_care_tab |>
    dplyr::filter(
      substr(
        gkz_join,
        start = 1L,
        stop = 2L
      ) != "08"
    ) |>
    dplyr::summarise(
      area_group = "Ausserhalb Baden-Wuerttembergs",
      n_tumors = sum(
        n_tumors,
        na.rm = TRUE
      ),
      n_near = sum(
        n_near,
        na.rm = TRUE
      ),
      n_not_near = sum(
        n_not_near,
        na.rm = TRUE
      ),
      share_near = safe_divide(
        n_near,
        n_tumors
      )
    )

  area_near_care_summary <- dplyr::bind_rows(
    inside_bw_summary,
    outside_bw_summary
  )

  area_near_care_masked <- mask_threshold_category(
    tab = area_near_care_summary,
    output_category = "map_area",
    n_col = c(
      "n_tumors",
      "n_near",
      "n_not_near"
    ),
    label_cols = "area_group",
    object_name = "04_03_area_near_care_summary"
  )$tab |>
    mask_share_after_threshold()

  ctx <- tc_write_table(
    tab = area_near_care_summary,
    table_id = "04_03_area_near_care_summary",
    ctx = ctx,
    output_category = "map_area",
    threshold = TRUE,
    threshold_n_col = c(
      "n_tumors",
      "n_near",
      "n_not_near"
    ),
    threshold_label_cols = "area_group"
  )


  ## Abbildung --------------------------

  p_near_care_area_bar <- area_near_care_masked |>
    dplyr::mutate(
      area_group = factor(
        area_group,
        levels = c(
          "Baden-Wuerttemberg",
          "Ausserhalb Baden-Wuerttembergs"
        )
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = area_group,
        y = share_near,
        fill = share_near
      )
    ) +
    ggplot2::geom_col(
      width = 0.65,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(
          share_near,
          accuracy = 1
        )
      ),
      vjust = -0.4,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(
        accuracy = 1
      ),
      expand = ggplot2::expansion(
        mult = c(0, 0.08)
      )
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      limits = c(0, 1),
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Anteil wohnortnaher Erstdiagnosen",
      subtitle = paste0(
        "Baden-Wuerttemberg und ausserhalb | ",
        "Wohnortnah bis ",
        config$analysis$near_care_km,
        " km"
      ),
      x = NULL,
      y = "Anteil wohnortnah"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 15,
        hjust = 1
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        face = "bold"
      )
    )

  # Die Abbildung basiert bereits auf der maskierten Tabelle.
  ctx <- tc_write_figure(
    plot = p_near_care_area_bar,
    fig_id = "04_03_area_near_care_bar",
    filename = "share_near_bw_vs_outside",
    ctx = ctx,
    threshold_tab = area_near_care_masked,
    output_category = "map_area",
    threshold = FALSE
  )


  # Kartenbasis -----------------------------
  ## Verfuegbarkeit --------------------------

  if (
    exists("gkz_geo") &&
      !is.null(gkz_geo) &&
      inherits(gkz_geo, "sf")
  ) {
    configured_gkz_key <- config$geodata$gkz_key_sf

    gkz_key_candidates <- unique(
      c(
        configured_gkz_key,
        "gemeinde_i",
        "GKZ",
        "gkz",
        "AGS",
        "ags"
      )
    )

    gkz_sf_key <- gkz_key_candidates[
      gkz_key_candidates %in% names(gkz_geo)
    ][1L]

    if (
      length(gkz_sf_key) == 0L ||
        is.na(gkz_sf_key)
    ) {
      stop(
        paste0(
          "In gkz_geo wurde keine geeignete GKZ-Variable gefunden. ",
          "Gepruefte Variablen: ",
          paste(
            gkz_key_candidates,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    gkz_geo_heatmap <- gkz_geo |>
      sf::st_transform(
        crs = config$geodata$crs_target
      ) |>
      dplyr::mutate(
        gkz_join = kr_normalize_gkz(
          .data[[gkz_sf_key]]
        )
      ) |>
      dplyr::filter(
        !is.na(gkz_join),
        gkz_join != "00000000",
        substr(
          gkz_join,
          start = 1L,
          stop = 2L
        ) == "08"
      )


    # Zuordnungsdiagnostik -----------------------------
    ## Nicht kartierte Diagnose-GKZ --------------------------

    bundesland_lookup <- c(
      "01" = "Schleswig-Holstein",
      "02" = "Hamburg",
      "03" = "Niedersachsen",
      "04" = "Bremen",
      "05" = "Nordrhein-Westfalen",
      "06" = "Hessen",
      "07" = "Rheinland-Pfalz",
      "08" = "Baden-Wuerttemberg",
      "09" = "Bayern",
      "10" = "Saarland",
      "11" = "Berlin",
      "12" = "Brandenburg",
      "13" = "Mecklenburg-Vorpommern",
      "14" = "Sachsen",
      "15" = "Sachsen-Anhalt",
      "16" = "Thueringen"
    )

    gkz_join_check <- gkz_near_care_tab |>
      dplyr::anti_join(
        gkz_geo_heatmap |>
          sf::st_drop_geometry() |>
          dplyr::distinct(
            gkz_join
          ),
        by = "gkz_join"
      ) |>
      dplyr::mutate(
        bundesland_id = substr(
          gkz_join,
          start = 1L,
          stop = 2L
        ),
        bundesland = unname(
          bundesland_lookup[bundesland_id]
        ),
        grund_nicht_kartiert = dplyr::case_when(
          bundesland_id != "08" ~
            "Ausserhalb Baden-Wuerttembergs",
          TRUE ~
            "GKZ nicht in der Gemeindedatei gefunden"
        )
      ) |>
      dplyr::select(
        gkz_join,
        bundesland_id,
        bundesland,
        grund_nicht_kartiert,
        n_tumors,
        n_near,
        n_not_near,
        share_near
      )

    ctx <- tc_write_table(
      tab = gkz_join_check,
      table_id = "04_03_gkz_not_mapped",
      ctx = ctx,
      output_category = "map_area",
      threshold = TRUE,
      threshold_n_col = c(
        "n_tumors",
        "n_near",
        "n_not_near"
      ),
      threshold_label_cols = c(
        "gkz_join",
        "bundesland",
        "grund_nicht_kartiert"
      )
    )


    # Kartenaggregation -----------------------------
    ## Ergebnisse und Geometrien --------------------------

    gkz_near_care_map_tab <- tumor_heatmap_base |>
      dplyr::filter(
        substr(
          gkz_join,
          start = 1L,
          stop = 2L
        ) == "08"
      ) |>
      dplyr::mutate(
        map_key = substr(
          gkz_join,
          start = 1L,
          stop = map_gkz_digits
        )
      ) |>
      dplyr::group_by(
        map_key
      ) |>
      dplyr::summarise(
        n_tumors = dplyr::n(),
        n_near = sum(
          near_care,
          na.rm = TRUE
        ),
        n_not_near = sum(
          !near_care,
          na.rm = TRUE
        ),
        share_near = safe_divide(
          n_near,
          n_tumors
        ),
        .groups = "drop"
      )

    gkz_near_care_map_masked <- mask_threshold_category(
      tab = gkz_near_care_map_tab,
      output_category = "map_area",
      n_col = c(
        "n_tumors",
        "n_near",
        "n_not_near"
      ),
      label_cols = "map_key",
      object_name = "04_03_gkz_near_care_heatmap"
    )$tab |>
      mask_share_after_threshold()

    ctx <- tc_write_table(
      tab = gkz_near_care_map_tab,
      table_id = paste0(
        "04_03_",
        tolower(map_level_label),
        "_near_care_share"
      ),
      ctx = ctx,
      output_category = "map_area",
      threshold = TRUE,
      threshold_n_col = c(
        "n_tumors",
        "n_near",
        "n_not_near"
      ),
      threshold_label_cols = "map_key"
    )

    gkz_geo_map <- gkz_geo_heatmap |>
      dplyr::mutate(
        map_key = substr(
          gkz_join,
          start = 1L,
          stop = map_gkz_digits
        )
      ) |>
      dplyr::group_by(
        map_key
      ) |>
      dplyr::summarise(
        .groups = "drop"
      ) |>
      sf::st_simplify(
        dTolerance =
          heatmap_config$simplify_tolerance_m,
        preserveTopology = TRUE
      )

    gkz_near_care_map <- gkz_geo_map |>
      dplyr::left_join(
        gkz_near_care_map_masked,
        by = "map_key"
      )

    # Konfigurierte Staedte als Orientierungspunkte verorten.
    city_points <- gkz_geo_heatmap |>
      dplyr::inner_join(
        heatmap_config$cities |>
          dplyr::mutate(
            gkz_join = kr_normalize_gkz(
              gkz_join
            )
          ),
        by = "gkz_join"
      ) |>
      dplyr::select(
        city_label,
        geometry
      ) |>
      sf::st_point_on_surface()

    n_mapped_areas <- gkz_near_care_map |>
      sf::st_drop_geometry() |>
      dplyr::filter(
        !is.na(n_tumors)
      ) |>
      dplyr::summarise(
        n = dplyr::n_distinct(
          map_key
        )
      ) |>
      dplyr::pull(
        n
      )

    message(
      "Aggregierte Diagnose-GKZ: ",
      nrow(gkz_near_care_tab)
    )

    message(
      "Nicht direkt zuordenbare Diagnose-GKZ: ",
      nrow(gkz_join_check)
    )

    message(
      "Tumoren ausserhalb Baden-Wuerttembergs: ",
      outside_bw_summary$n_tumors
    )

    message(
      "Kartierte ",
      map_level_plural,
      " mit auswertbaren Tumordaten: ",
      n_mapped_areas
    )


    # Heatmap -----------------------------
    ## Abbildung --------------------------

    p_near_care_heatmap <- ggplot2::ggplot(
      gkz_near_care_map
    ) +
      ggplot2::geom_sf(
        ggplot2::aes(
          fill = share_near
        ),
        color = heatmap_config$boundary_color,
        linewidth = map_linewidth
      ) +
      ggplot2::geom_sf(
        data = city_points,
        shape = 21,
        size = heatmap_config$city_point_size,
        stroke = heatmap_config$city_point_stroke,
        color = "black",
        fill = "white",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_sf_text(
        data = city_points,
        ggplot2::aes(
          label = city_label
        ),
        nudge_y = heatmap_config$city_label_nudge_y,
        size = heatmap_config$city_label_size,
        fontface = "bold",
        color = "black",
        check_overlap = TRUE,
        inherit.aes = FALSE
      ) +
      ggplot2::coord_sf(
        datum = NA
      ) +
      ggplot2::scale_fill_viridis_c(
        option = "C",
        limits = c(0, 1),
        labels = scales::label_percent(
          accuracy = 1
        ),
        na.value = "grey90",
        name = "Anteil\nwohnortnah"
      ) +
      ggplot2::labs(
        title = paste0(
          "Anteil wohnortnaher Erstdiagnosen nach ",
          map_level_label
        ),
        subtitle = paste0(
          "Baden-Wuerttemberg | ",
          "Wohnortnah bis ",
          config$analysis$near_care_km,
          " km | ",
          outside_bw_summary$n_tumors,
          " Tumoren ausserhalb separat zusammengefasst"
        )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          face = "bold"
        ),
        legend.title = ggplot2::element_text(
          face = "bold"
        )
      )

    # Kein print(): Die Datei wird direkt gespeichert, ohne das zeitintensive
    # Rendern im RStudio-Plotfenster anzustoßen.
    ctx <- tc_write_figure(
      plot = p_near_care_heatmap,
      fig_id = "04_03_gkz_near_care_heatmap",
      filename = paste0(
        "share_near_",
        tolower(map_level_label)
      ),
      ctx = ctx,
      threshold_tab = gkz_near_care_map_masked,
      output_category = "map_area",
      threshold = FALSE
    )
  } else {
    # Nur die Karte ueberspringen; der Gebietsvergleich wurde bereits erzeugt.
    heatmap_status <- data.frame(
      object = "04_03_gkz_near_care_heatmap",
      status = "uebersprungen",
      reason = "Keine GKZ-Geodaten vorhanden",
      analysis_group =
        config$analysis$active_output_group,
      stringsAsFactors = FALSE
    )

    ctx <- tc_write_table(
      tab = heatmap_status,
      table_id = "04_03_heatmap_status",
      ctx = ctx,
      output_category = "technical_overview",
      threshold = FALSE
    )
  }
}
