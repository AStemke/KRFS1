# Stuttgart-Heatmap -----------------------------
## Zweck --------------------------
# Stellt den Anteil wohnortnah behandelter Tumoren nur fuer Diagnose-GKZ mit
# Prefix 0811 dar. Gegenueber der BW-Karte wird auf die achtstellige GKZ-Ebene
# und damit auf die feinste Ebene der aktuell eingebundenen Gemeindedatei gegangen.

# Konfiguration -----------------------------
## Kartenparameter --------------------------
heatmap_config <- config$geodata$heatmap
stuttgart_config <- heatmap_config$stuttgart
map_gkz_digits <- as.integer(stuttgart_config$gkz_digits)
map_prefix <- stuttgart_config$gkz_prefix
map_linewidth <- unname(
  heatmap_config$line_width_by_digits[
    as.character(map_gkz_digits)
  ]
)

# Datenbasis -----------------------------
## Diagnose-GKZ 0811 --------------------------
tumor_stuttgart_heatmap <- tumor_base |>
  dplyr::mutate(
    gkz_join = kr_normalize_gkz(gkz_diagnose)
  ) |>
  dplyr::filter(
    !is.na(gkz_join),
    startsWith(gkz_join, map_prefix),
    !is.na(distance_km),
    !is.na(near_care)
  ) |>
  dplyr::mutate(
    map_key = substr(
      gkz_join,
      start = 1L,
      stop = map_gkz_digits
    )
  )

stuttgart_near_care_tab <- tumor_stuttgart_heatmap |>
  dplyr::group_by(map_key) |>
  dplyr::summarise(
    n_tumors = dplyr::n(),
    n_near = sum(near_care, na.rm = TRUE),
    n_not_near = sum(!near_care, na.rm = TRUE),
    share_near = safe_divide(n_near, n_tumors),
    .groups = "drop"
  )

stuttgart_near_care_masked <- mask_threshold_category(
  tab = stuttgart_near_care_tab,
  output_category = "map_area",
  n_col = c("n_tumors", "n_near", "n_not_near"),
  label_cols = "map_key",
  object_name = "04_04_stuttgart_near_care_heatmap"
)$tab |>
  dplyr::mutate(
    share_near = dplyr::if_else(
      is.na(n_tumors) | is.na(n_near) | is.na(n_not_near),
      NA_real_,
      as.numeric(share_near)
    )
  )

ctx <- tc_write_table(
  tab = stuttgart_near_care_tab,
  table_id = "04_04_stuttgart_gkz_near_care_share",
  ctx = ctx,
  output_category = "map_area",
  threshold = TRUE,
  threshold_n_col = c("n_tumors", "n_near", "n_not_near"),
  threshold_label_cols = "map_key"
)

# Kartenbasis -----------------------------
## Geometrien auf GKZ 0811 begrenzen --------------------------
if (
  exists("gkz_geo") &&
    !is.null(gkz_geo) &&
    inherits(gkz_geo, "sf")
) {
  configured_gkz_key <- config$geodata$gkz_key_sf
  gkz_key_candidates <- unique(
    c(configured_gkz_key, "gemeinde_i", "GKZ", "gkz", "AGS", "ags")
  )
  gkz_sf_key <- gkz_key_candidates[
    gkz_key_candidates %in% names(gkz_geo)
  ][1L]

  if (length(gkz_sf_key) == 0L || is.na(gkz_sf_key)) {
    stop(
      "In gkz_geo wurde keine geeignete GKZ-Variable fuer die Stuttgart-Heatmap gefunden.",
      call. = FALSE
    )
  }

  gkz_geo_stuttgart <- gkz_geo |>
    sf::st_transform(crs = config$geodata$crs_target) |>
    dplyr::mutate(
      gkz_join = kr_normalize_gkz(.data[[gkz_sf_key]])
    ) |>
    dplyr::filter(
      !is.na(gkz_join),
      startsWith(gkz_join, map_prefix)
    ) |>
    dplyr::mutate(
      map_key = substr(
        gkz_join,
        start = 1L,
        stop = map_gkz_digits
      )
    ) |>
    dplyr::group_by(map_key) |>
    dplyr::summarise(.groups = "drop") |>
    sf::st_simplify(
      dTolerance = heatmap_config$simplify_tolerance_m,
      preserveTopology = TRUE
    )

  stuttgart_near_care_map <- gkz_geo_stuttgart |>
    dplyr::left_join(
      stuttgart_near_care_masked,
      by = "map_key"
    )

  # Diagnose-GKZ, die trotz Prefix 0811 nicht in der Geometriedatei liegen.
  stuttgart_not_mapped <- stuttgart_near_care_tab |>
    dplyr::anti_join(
      gkz_geo_stuttgart |>
        sf::st_drop_geometry() |>
        dplyr::distinct(map_key),
      by = "map_key"
    )

  ctx <- tc_write_table(
    tab = stuttgart_not_mapped,
    table_id = "04_04_stuttgart_gkz_not_mapped",
    ctx = ctx,
    output_category = "map_area",
    threshold = TRUE,
    threshold_n_col = c("n_tumors", "n_near", "n_not_near"),
    threshold_label_cols = "map_key"
  )

  # Heatmap -----------------------------
  ## Abbildung --------------------------
  p_near_care_stuttgart <- ggplot2::ggplot(
    stuttgart_near_care_map
  ) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = share_near),
      color = heatmap_config$boundary_color,
      linewidth = map_linewidth
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1),
      na.value = "grey90",
      name = "Anteil\nwohnortnah"
    ) +
    ggplot2::labs(
      title = "Anteil wohnortnaher Versorgung innerhalb GKZ 0811",
      subtitle = paste0(
        "Achtstellige GKZ-Ebene | Default-Distanz ",
        config$analysis$near_care_default_km,
        " km; entitaetsspezifische Werte haben Vorrang"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold")
    )

  ctx <- tc_write_figure(
    plot = p_near_care_stuttgart,
    fig_id = "04_04_gkz_near_care_heatmap",
    filename = "share_near_stuttgart_0811_gkz8",
    ctx = ctx,
    threshold_tab = stuttgart_near_care_masked,
    output_category = "map_area",
    threshold = FALSE
  )
} else {
  heatmap_status <- data.frame(
    object = "04_04_gkz_near_care_heatmap",
    status = "uebersprungen",
    reason = "Keine GKZ-Geodaten vorhanden",
    analysis_group = config$analysis$active_output_group,
    stringsAsFactors = FALSE
  )

  ctx <- tc_write_table(
    tab = heatmap_status,
    table_id = "04_04_stuttgart_heatmap_status",
    ctx = ctx,
    output_category = "technical_overview",
    threshold = FALSE
  )
}
