# 04_01_03_GKZ_Near_Care_Heatmap.R -----------------------------
## Zweck --------------------------
# Choroplethenkarte des Anteils wohnortnah behandelter Tumoren.
#
# Standardmaessig werden die Tumordaten anhand der ersten fuenf Stellen
# der Diagnose-GKZ auf Kreis- beziehungsweise Stadtkreisebene aggregiert.
#
# Alternativ kann mit map_gkz_digits <- 8L eine Gemeindekarte erstellt werden.
#
# Die Karte wird fuer folgende Analysegruppen erzeugt:
# - Selektiv_Gesamt
# - C50
# - C61
# - C34
# - C18
# - C44
#
# Als wohnortnah gilt:
# distance_km <= config$analysis$near_care_km
#
# Tumoren ohne Distanzangabe oder eindeutige Zuordnung zur wohnortnahen
# Versorgung gehen nicht in den Nenner ein.
#
# Ausserhalb Baden-Wuerttembergs gelegene Tumoren werden nicht in der
# Baden-Wuerttemberg-Karte dargestellt, aber separat tabellarisch und
# grafisch zusammengefasst.


# Hilfsfunktionen -----------------------------
## Gemeindeschluessel vereinheitlichen --------------------------

normalize_gkz <- function(x) {
  
  x <- as.character(x)
  x <- trimws(x)
  
  # Eventuelle Dezimalstellen numerisch importierter GKZ entfernen.
  x <- sub(
    pattern = "\\.0+$",
    replacement = "",
    x = x
  )
  
  # Leere Werte entfernen.
  x[x == ""] <- NA_character_
  
  # Amtlichen Gemeindeschluessel auf acht Stellen auffuellen.
  x <- stringr::str_pad(
    string = x,
    width = 8L,
    side = "left",
    pad = "0"
  )
  
  # Nullschluessel nicht als gueltige GKZ behandeln.
  x[x == "00000000"] <- NA_character_
  
  x
}


## Anteile nach Threshold-Maskierung absichern --------------------------

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


# Zielkohorten -----------------------------
## Heatmap nur fuer selektive und fokussierte Auswertungen --------------------------

heatmap_groups <- c(
  "Selektiv_Gesamt",
  config$analysis$focus_icd10_3
)


if (config$analysis$active_output_group %in% heatmap_groups) {
  
  # Kartenparameter -----------------------------
  ## Kartenebene festlegen --------------------------
  
  # 5 Stellen:
  # Kreis beziehungsweise Stadtkreis.
  #
  # 8 Stellen:
  # Gemeinde.
  map_gkz_digits <- 5L
  
  stopifnot(
    map_gkz_digits %in% c(5L, 8L)
  )
  
  map_level_label <- dplyr::case_when(
    map_gkz_digits == 5L ~ "Kreis",
    map_gkz_digits == 8L ~ "Gemeinde"
  )
  
  map_level_plural <- dplyr::case_when(
    map_gkz_digits == 5L ~ "Kreise",
    map_gkz_digits == 8L ~ "Gemeinden"
  )
  
  map_linewidth <- dplyr::case_when(
    map_gkz_digits == 5L ~ 0.15,
    map_gkz_digits == 8L ~ 0.025
  )
  
  
  # Titelstruktur -----------------------------
  ## Aktive Analysegruppe fuer Abbildungen benennen --------------------------
  
  analysis_scope_label <- dplyr::case_when(
    config$analysis$active_output_group == "Gesamt" ~
      "Komplett",
    
    config$analysis$active_output_group == "Selektiv_Gesamt" ~
      "Gesamt",
    
    TRUE ~
      paste0(
        "ICD ",
        config$analysis$active_output_group
      )
  )
  
  
  # Eingangsdaten pruefen -----------------------------
  ## Erforderliche Objekte und Variablen kontrollieren --------------------------
  
  stopifnot(
    exists("tumor_base"),
    "gkz_diagnose" %in% names(tumor_base),
    "distance_km" %in% names(tumor_base),
    "near_care" %in% names(tumor_base)
  )
  
  
  # Tumordaten vorbereiten -----------------------------
  ## Diagnose-GKZ und Distanzangaben vereinheitlichen --------------------------
  
  tumor_heatmap_base <- tumor_base |>
    dplyr::mutate(
      gkz_join = normalize_gkz(
        gkz_diagnose
      )
    ) |>
    dplyr::filter(
      !is.na(gkz_join),
      !is.na(distance_km),
      !is.na(near_care)
    )
  
  
  # Aggregation nach Gemeinde-GKZ -----------------------------
  ## Wohnortnahe Versorgung auf achtstelliger GKZ-Ebene berechnen --------------------------
  
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
  
  
  # GKZ-Tabelle speichern -----------------------------
  ## Gemeindeergebnisse datenschutzgeprueft ausgeben --------------------------
  
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
  
  
  # Geodaten pruefen -----------------------------
  ## Karte nur bei vorhandenen GKZ-Geometrien erstellen --------------------------
  
  if (
    exists("gkz_geo") &&
    !is.null(gkz_geo) &&
    inherits(gkz_geo, "sf")
  ) {
    
    # Geodatenschluessel bestimmen -----------------------------
    ## Konfigurierten oder vorhandenen GKZ-Schluessel verwenden --------------------------
    
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
    
    
    # Geodaten vorbereiten -----------------------------
    ## Projektion und achtstellige GKZ vereinheitlichen --------------------------
    
    gkz_geo_heatmap <- gkz_geo |>
      sf::st_transform(
        crs = config$geodata$crs_target
      ) |>
      dplyr::mutate(
        gkz_join = normalize_gkz(
          .data[[gkz_sf_key]]
        )
      ) |>
      dplyr::filter(
        !is.na(gkz_join),
        substr(
          gkz_join,
          start = 1L,
          stop = 2L
        ) == "08"
      )
    
    
    # Bundeslandzuordnung -----------------------------
    ## Bundesland anhand der ersten beiden GKZ-Stellen bestimmen --------------------------
    
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
    
    
    # Join-Diagnostik -----------------------------
    ## Nicht in der Gemeindedatei enthaltene Diagnose-GKZ bestimmen --------------------------
    
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
    
    
    # Join-Diagnostik speichern -----------------------------
    ## Nicht direkt zuordenbare GKZ datenschutzgeprueft ausgeben --------------------------
    
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
    
    
    # Gebietszusammenfassung -----------------------------
    ## Baden-Wuerttemberg und ausserhalb zusammenfassen --------------------------
    
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
    
    
    # Gebietszusammenfassung maskieren -----------------------------
    ## Anteile bei zu kleinen Fallzahlen ebenfalls unterdruecken --------------------------
    
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
    
    
    # Gebietszusammenfassung speichern -----------------------------
    ## Baden-Wuerttemberg und ausserhalb tabellarisch ausgeben --------------------------
    
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
    
    
    # Vergleichsdiagramm erstellen -----------------------------
    ## Baden-Wuerttemberg mit ausserhalb gelegenen Tumoren vergleichen --------------------------
    
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
        width = 0.65
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(
            is.na(share_near),
            "Unterdrueckt",
            scales::percent(
              share_near,
              accuracy = 1
            )
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
        title = paste0(
          analysis_scope_label,
          ": Anteil wohnortnaher Versorgung"
        ),
        subtitle = paste0(
          "Vergleich Baden-Wuerttemberg und ausserhalb; ",
          "wohnortnah bis ",
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
    
    
    # Vergleichsdiagramm speichern -----------------------------
    ## Datenschutzgepruefte Abbildung ausgeben --------------------------
    
    ctx <- tc_write_figure(
      plot = p_near_care_area_bar,
      fig_id = "04_03_area_near_care_bar",
      filename = "share_near_bw_vs_outside",
      ctx = ctx,
      threshold_tab = area_near_care_masked,
      output_category = "map_area",
      threshold_n_col = c(
        "n_tumors",
        "n_near",
        "n_not_near"
      ),
      threshold_label_cols = "area_group"
    )
    
    
    # Kartenaggregation -----------------------------
    ## Tumordaten auf Kreis- oder Gemeindeebene zusammenfassen --------------------------
    
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
    
    
    # Kartenaggregation maskieren -----------------------------
    ## Kleine Gebietsfallzahlen vor dem Kartenjoin unterdruecken --------------------------
    
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
    
    
    # Kartenaggregation speichern -----------------------------
    ## Ergebnisse der dargestellten Kartenebene ausgeben --------------------------
    
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
    
    
    # Kartenflaechen aggregieren -----------------------------
    ## Gemeindegeometrien zu Kreisen oder Gemeinden zusammenfassen --------------------------
    
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
        dTolerance = 200,
        preserveTopology = TRUE
      )
    
    
    # Orientierungspunkte vorbereiten -----------------------------
    ## Stuttgart, Freiburg und Tuebingen markieren --------------------------
    
    city_lookup <- tibble::tribble(
      ~gkz_join,  ~city_label,
      "08111000", "Stuttgart",
      "08311000", "Freiburg",
      "08416041", "Tuebingen"
    )
    
    city_points <- gkz_geo_heatmap |>
      dplyr::inner_join(
        city_lookup,
        by = "gkz_join"
      ) |>
      dplyr::select(
        city_label,
        geometry
      ) |>
      sf::st_point_on_surface()
    
    
    # Kartenwerte verbinden -----------------------------
    ## Datenschutzgepruefte Ergebnisse an die Kartenflaechen anfuegen --------------------------
    
    gkz_near_care_map <- gkz_geo_map |>
      dplyr::left_join(
        gkz_near_care_map_masked,
        by = "map_key"
      )
    
    
    # Kartierungsdiagnostik -----------------------------
    ## Anzahl dargestellter Gebiete bestimmen --------------------------
    
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
    
    
    # Karte erstellen -----------------------------
    ## Anteil wohnortnaher Versorgung kartografisch darstellen --------------------------
    
    p_near_care_heatmap <- ggplot2::ggplot(
      gkz_near_care_map
    ) +
      ggplot2::geom_sf(
        ggplot2::aes(
          fill = share_near
        ),
        color = "grey70",
        linewidth = map_linewidth
      ) +
      ggplot2::geom_sf(
        data = city_points,
        shape = 21,
        size = 2.4,
        stroke = 0.6,
        color = "black",
        fill = "white",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_sf_text(
        data = city_points,
        ggplot2::aes(
          label = city_label
        ),
        nudge_y = 5500,
        size = 3.2,
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
          analysis_scope_label,
          ": Anteil wohnortnaher Versorgung nach ",
          map_level_label
        ),
        subtitle = paste0(
          "Baden-Wuerttemberg; wohnortnah bis ",
          config$analysis$near_care_km,
          " km; ",
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
    
    
    # Karte speichern -----------------------------
    ## Datenschutzgepruefte Abbildung ausgeben --------------------------
    
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
      threshold_n_col = c(
        "n_tumors",
        "n_near",
        "n_not_near"
      ),
      threshold_label_cols = "map_key"
    )
    
  } else {
    
    # Statusausgabe -----------------------------
    ## Fehlende Geodaten dokumentieren --------------------------
    
    heatmap_status <- data.frame(
      object = "04_03_gkz_near_care_heatmap",
      status = "uebersprungen",
      reason = "Keine GKZ-Geodaten vorhanden",
      analysis_group = config$analysis$active_output_group,
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