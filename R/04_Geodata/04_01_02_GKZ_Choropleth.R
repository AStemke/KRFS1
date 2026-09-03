# Wohnort-Choropleth -----------------------------
## Zweck --------------------------
# Erstellt bei vorhandenen Gemeindegeometrien eine Karte der Tumorfallzahl
# nach konfigurierter Wohn-GKZ. Kleine Gebietsfallzahlen werden vor dem Kartenjoin
# zeilenweise maskiert. Die dargestellte Geometrie wird auf die aktive Wohnregion
# begrenzt; Krankenhausstandorte bleiben davon unberuehrt.

# Fallzahlkarte -----------------------------
## Kartendaten --------------------------
if (
  exists("gkz_geo") &&
    !is.null(gkz_geo) &&
    inherits(gkz_geo, "sf") &&
    exists("gkz_residence_tab")
) {
  region_config <- config$analysis$analysis_region
  sf_key <- config$geodata$gkz_key_sf

  if (!sf_key %in% names(gkz_geo)) {
    stop(
      paste0(
        "Fuer die Wohnortkarte fehlt die Geodaten-GKZ `",
        sf_key,
        "`."
      ),
      call. = FALSE
    )
  }

  gkz_geo_region <- gkz_geo |>
    dplyr::mutate(
      .gkz_region = kr_normalize_gkz(.data[[sf_key]])
    )

  if (isTRUE(region_config$enabled)) {
    gkz_geo_region <- gkz_geo_region |>
      dplyr::filter(
        !is.na(.gkz_region),
        startsWith(.gkz_region, region_config$gkz_prefix)
      )
  }

  gkz_geo_region <- gkz_geo_region |>
    dplyr::select(-.gkz_region)

  gkz_map_tab <- mask_threshold_category(
    tab = gkz_residence_tab,
    output_category = "map_area",
    n_col = c(
      "n_tumors",
      "n_patients"
    ),
    label_cols = "gkz_residence",
    object_name = "04_02_gkz_choropleth"
  )$tab

  gkz_map <- join_geo_counts(
    sf_obj = gkz_geo_region,
    count_tab = gkz_map_tab,
    sf_key = sf_key,
    tab_key = "gkz_residence"
  )

  ## Abbildung --------------------------
  p_map <- plot_choropleth(
    sf_obj = gkz_map,
    fill_var = "n_tumors",
    title = paste0(
      "Tumoren nach aktuellem Wohnort | ",
      kr_analysis_region_label()
    )
  )

  # Die Datengrundlage ist bereits maskiert; eine erneute globale
  # Threshold-Pruefung wuerde die gesamte Abbildung unnoetig unterdruecken.
  ctx <- tc_write_figure(
    plot = p_map,
    fig_id = "04_02_gkz_choropleth",
    filename = "n_tumors_residence",
    ctx = ctx,
    threshold_tab = gkz_map_tab,
    output_category = "map_area",
    threshold = FALSE
  )
} else {
  # Das Fehlen der Kartengrundlage technisch dokumentieren.
  geodata_status <- data.frame(
    object = "04_02_gkz_choropleth",
    status = "uebersprungen",
    reason = paste0(
      "Keine Geodaten vorhanden oder ",
      "keine Wohnortaggregation erzeugt"
    ),
    stringsAsFactors = FALSE
  )

  ctx <- tc_write_table(
    tab = geodata_status,
    table_id = "04_02_geodata_status",
    ctx = ctx,
    output_category = "technical_overview",
    threshold = FALSE
  )
}
