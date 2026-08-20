# 04_01_01_GKZ_Diagnosis_Aggregate.R -----------
# Zweck: Aggregation nach Gemeindekennziffer zum Diagnosezeitpunkt

gkz_diag_tab <- tumor_base |>
  dplyr::filter(!is.na(gkz_diagnose)) |>
  dplyr::group_by(gkz_diagnose) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    median_distance_km = stats::median(distance_km, na.rm = TRUE),
    pct_near_care = mean(near_care, na.rm = TRUE),
    .groups = "drop"
  )

ctx <- tc_write_table(
  tab = gkz_diag_tab,
  table_id = "04_01_gkz_diagnosis_aggregate",
  ctx = ctx,
  output_category = "map_area",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("gkz_diagnose")
)
