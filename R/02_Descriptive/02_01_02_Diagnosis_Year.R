# 02_01_02_Diagnosis_Year.R -----------
# Zweck: Faelle nach Diagnosejahr

year_tab <- tumor_base |>
  dplyr::filter(!is.na(diagnosis_year)) |>
  dplyr::group_by(diagnosis_year) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    .groups = "drop"
  ) |>
  tidyr::complete(
    diagnosis_year = seq(min(diagnosis_year, na.rm = TRUE), max(diagnosis_year, na.rm = TRUE)),
    fill = list(n_tumors = 0L, n_patients = 0L)
  )

ctx <- tc_write_table(
  tab = year_tab,
  table_id = "02_02_diagnosis_year",
  ctx = ctx,
  output_category = "aggregate_table",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("diagnosis_year")
)

p_year <- ggplot2::ggplot(year_tab, ggplot2::aes(x = diagnosis_year, y = n_tumors)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Tumoren nach Diagnosejahr", x = "Diagnosejahr", y = "Anzahl Tumoren") +
  ggplot2::theme_minimal()

ctx <- tc_write_figure(
  plot = p_year,
  fig_id = "02_02_diagnosis_year",
  filename = "bar",
  ctx = ctx,
  threshold_tab = year_tab,
  output_category = "aggregate_figure",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("diagnosis_year")
)
