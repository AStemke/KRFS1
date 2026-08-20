# 03_01_03_Distance_By_ICD.R -----------
# Zweck: Distanzgruppen nach ICD-10-3-Steller

distance_icd_tab <- tumor_base |>
  dplyr::filter(!is.na(icd10_3)) |>
  dplyr::mutate(distance_group = factor(as.character(distance_group), levels = config$analysis$distance_labels)) |>
  dplyr::group_by(icd10_3, distance_group) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    .groups = "drop"
  ) |>
  tidyr::complete(
    icd10_3,
    distance_group = factor(config$analysis$distance_labels, levels = config$analysis$distance_labels),
    fill = list(n_tumors = 0L, n_patients = 0L)
  )

ctx <- tc_write_table(
  tab = distance_icd_tab,
  table_id = "03_03_distance_by_icd10",
  ctx = ctx,
  output_category = "crosstab",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("icd10_3", "distance_group")
)
