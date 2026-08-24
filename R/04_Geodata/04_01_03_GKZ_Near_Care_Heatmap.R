# 04_01_03_GKZ_Near_Care_Heatmap.R -----------------------------
## Zweck --------------------------
# Choroplethen-Heatmap des Anteils wohnortnah behandelter Tumoren nach Diagnose-GKZ.
# Die Karte wird fuer Selektiv_Gesamt sowie C50, C61, C34, C18 und C44 erzeugt.
# Als wohnortnah gilt Distanz_km <= config$analysis$near_care_km.
# Tumoren ohne Distanzangabe gehen nicht in den Nenner des Anteils ein.

# Zielkohorten -----------------------------
## Heatmap nur fuer selektive und fokussierte Auswertungen --------------------------
heatmap_groups <- c("Selektiv_Gesamt", config$analysis$focus_icd10_3)

if (config$analysis$active_output_group %in% heatmap_groups) {

  # Aggregation -----------------------------
  ## Wohnortnahe Versorgung nach Diagnose-GKZ --------------------------
  gkz_near_care_tab <- tumor_base |>
    dplyr::filter(!is.na(gkz_diagnose), !is.na(distance_km)) |>
    dplyr::group_by(gkz_diagnose) |>
    dplyr::summarise(
      n_tumors = dplyr::n(),
      n_near = sum(near_care, na.rm = TRUE),
      n_not_near = sum(!near_care, na.rm = TRUE),
      share_near = safe_divide(n_near, n_tumors),
      .groups = "drop"
    )

  # Datenschutz -----------------------------
  ## Kleine Zellen fuer Tabelle und Karte maskieren --------------------------
  gkz_near_care_masked <- mask_threshold_category(
    tab = gkz_near_care_tab,
    output_category = "map_area",
    n_col = c("n_tumors", "n_near", "n_not_near"),
    label_cols = "gkz_diagnose",
    object_name = "04_03_gkz_near_care_heatmap"
  )$tab

  ctx <- tc_write_table(
    tab = gkz_near_care_tab,
    table_id = "04_03_gkz_near_care_share",
    ctx = ctx,
    output_category = "map_area",
    threshold = TRUE,
    threshold_n_col = c("n_tumors", "n_near", "n_not_near"),
    threshold_label_cols = "gkz_diagnose"
  )

  # Heatmap -----------------------------
  ## Anteil wohnortnaher Versorgung kartografisch darstellen --------------------------
  if (exists("gkz_geo") && !is.null(gkz_geo)) {
    gkz_near_care_map <- join_geo_counts(
      sf_obj = gkz_geo,
      count_tab = gkz_near_care_masked,
      sf_key = config$geodata$gkz_key_sf,
      tab_key = "gkz_diagnose"
    )

    p_near_care_heatmap <- ggplot2::ggplot(gkz_near_care_map) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = share_near),
        color = NA
      ) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::scale_fill_viridis_c(
        option = "C",
        limits = c(0, 1),
        labels = scales::label_percent(accuracy = 1),
        na.value = "grey90",
        name = "Anteil wohnortnah"
      ) +
      ggplot2::labs(
        title = "Anteil wohnortnaher Versorgung nach Gemeinde"
      ) +
      ggplot2::theme_minimal()

    ctx <- tc_write_figure(
      plot = p_near_care_heatmap,
      fig_id = "04_03_gkz_near_care_heatmap",
      filename = "share_near",
      ctx = ctx,
      threshold_tab = gkz_near_care_masked,
      output_category = "map_area",
      threshold_n_col = c("n_tumors", "n_near", "n_not_near"),
      threshold_label_cols = "gkz_diagnose"
    )
  } else {
    heatmap_status <- data.frame(
      object = "04_03_gkz_near_care_heatmap",
      status = "uebersprungen",
      reason = "Keine GKZ-Geodaten vorhanden",
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
