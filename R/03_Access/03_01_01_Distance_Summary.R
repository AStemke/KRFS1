# 03_01_01_Distance_Summary.R -----------
# Zweck: Distanzkennzahlen insgesamt und nach Diagnosejahr

dist_overall <- tumor_base |>
  dplyr::summarise(
    group = "Gesamt",
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    n_non_missing = sum(!is.na(distance_km)),
    median_km = stats::median(distance_km, na.rm = TRUE),
    mean_km = mean(distance_km, na.rm = TRUE),
    q25_km = stats::quantile(distance_km, 0.25, na.rm = TRUE),
    q75_km = stats::quantile(distance_km, 0.75, na.rm = TRUE),
    max_km = suppressWarnings(max(distance_km, na.rm = TRUE))
  )

dist_year <- tumor_base |>
  dplyr::filter(!is.na(diagnosis_year)) |>
  dplyr::group_by(group = as.character(diagnosis_year)) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    n_non_missing = sum(!is.na(distance_km)),
    median_km = stats::median(distance_km, na.rm = TRUE),
    mean_km = mean(distance_km, na.rm = TRUE),
    q25_km = stats::quantile(distance_km, 0.25, na.rm = TRUE),
    q75_km = stats::quantile(distance_km, 0.75, na.rm = TRUE),
    max_km = suppressWarnings(max(distance_km, na.rm = TRUE)),
    .groups = "drop"
  )

dist_summary <- dplyr::bind_rows(dist_overall, dist_year)

ctx <- tc_write_table(
  tab = dist_summary,
  table_id = "03_01_distance_summary",
  ctx = ctx,
  output_category = "descriptive_numeric",
  threshold_n_col = c("n_tumors", "n_patients", "n_non_missing"),
  threshold_label_cols = c("group")
)
