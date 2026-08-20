# 04_01_02_GKZ_Choropleth.R -----------
# Zweck: Karte der Fallzahl nach GKZ, falls Geodaten vorhanden sind

if (exists("gkz_geo") && !is.null(gkz_geo) && exists("gkz_diag_tab")) {
  gkz_map_tab <- mask_threshold_category(
    tab = gkz_diag_tab,
    output_category = "map_area",
    n_col = c("n_tumors", "n_patients"),
    label_cols = c("gkz_diagnose"),
    object_name = "04_02_gkz_choropleth"
  )$tab
  
  gkz_map <- join_geo_counts(
    sf_obj = gkz_geo,
    count_tab = gkz_map_tab,
    sf_key = config$geodata$gkz_key_sf,
    tab_key = "gkz_diagnose"
  )
  
  p_map <- plot_choropleth(
    sf_obj = gkz_map,
    fill_var = "n_tumors",
    title = "Tumoren nach Gemeinde zum Diagnosezeitpunkt"
  )
  
  ctx <- tc_write_figure(
    plot = p_map,
    fig_id = "04_02_gkz_choropleth",
    filename = "n_tumors",
    ctx = ctx,
    threshold_tab = gkz_map_tab,
    output_category = "map_area",
    threshold_n_col = c("n_tumors", "n_patients"),
    threshold_label_cols = c("gkz_diagnose")
  )
} else {
  geodata_status <- data.frame(
    object = "04_02_gkz_choropleth",
    status = "uebersprungen",
    reason = "Keine Geodaten vorhanden oder keine Aggregation erzeugt",
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
