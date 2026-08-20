# 02_01_05_Therapy.R -----------
# Zweck: Dokumentierte Therapiearten auf Tumorebene

therapy_tab <- tumor_base |>
  dplyr::summarise(
    n_op = sum(has_op, na.rm = TRUE),
    n_st = sum(has_st, na.rm = TRUE),
    n_sy = sum(has_sy, na.rm = TRUE),
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR)
  ) |>
  tidyr::pivot_longer(
    cols = c("n_op", "n_st", "n_sy"),
    names_to = "therapy_indicator",
    values_to = "n"
  ) |>
  dplyr::mutate(
    therapy_indicator = dplyr::recode(
      therapy_indicator,
      n_op = "OP dokumentiert",
      n_st = "Strahlentherapie dokumentiert",
      n_sy = "Systemische Therapie dokumentiert"
    )
  )

ctx <- tc_write_table(
  tab = therapy_tab,
  table_id = "02_05_therapy_indicators",
  ctx = ctx,
  output_category = "aggregate_table",
  threshold_n_col = c("n", "n_tumors", "n_patients"),
  threshold_label_cols = c("therapy_indicator")
)
