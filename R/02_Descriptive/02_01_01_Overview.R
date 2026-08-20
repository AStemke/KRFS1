# 02_01_01_Overview.R -----------
# Zweck: Gesamtueberblick mit Patienten- und Tumorzahl

overview_tab <- data.frame(
  n_patients = dplyr::n_distinct(tumor_base$LNGREGISTERNR, na.rm = TRUE),
  n_tumors = dplyr::n_distinct(tumor_base$LNGTUMORID, na.rm = TRUE),
  n_messages = sum(tumor_base$n_messages, na.rm = TRUE),
  first_diagnosis_year = suppressWarnings(min(tumor_base$diagnosis_year, na.rm = TRUE)),
  last_diagnosis_year = suppressWarnings(max(tumor_base$diagnosis_year, na.rm = TRUE))
) |>
  dplyr::mutate(
    first_diagnosis_year = dplyr::if_else(is.infinite(first_diagnosis_year), NA_integer_, as.integer(first_diagnosis_year)),
    last_diagnosis_year = dplyr::if_else(is.infinite(last_diagnosis_year), NA_integer_, as.integer(last_diagnosis_year))
  )

ctx <- tc_write_table(
  tab = overview_tab,
  table_id = "02_01_overview",
  ctx = ctx,
  output_category = "aggregate_table",
  threshold_n_col = c("n_patients", "n_tumors"),
  threshold_label_cols = character(0)
)
