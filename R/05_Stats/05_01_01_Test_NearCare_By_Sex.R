# 05_01_01_Test_NearCare_By_Sex.R -----------
# Zweck: Kreuztabelle und Test: wohnortnah (<= near_care_km) nach Geschlecht

near_sex_tab <- tumor_base |>
  dplyr::filter(!is.na(sex), !is.na(near_care)) |>
  dplyr::mutate(near_care = ifelse(near_care, "wohnortnah", "nicht_wohnortnah")) |>
  dplyr::group_by(sex, near_care) |>
  dplyr::summarise(n = dplyr::n_distinct(LNGTUMORID), .groups = "drop") |>
  tidyr::complete(sex, near_care = c("wohnortnah", "nicht_wohnortnah"), fill = list(n = 0L))

ctx <- tc_write_table(
  tab = near_sex_tab,
  table_id = "05_01_nearcare_by_sex_crosstab",
  ctx = ctx,
  output_category = "crosstab",
  threshold_n_col = "n",
  threshold_label_cols = c("sex", "near_care")
)

if (passes_threshold_category(near_sex_tab, "crosstab", n_col = "n", label_cols = c("sex", "near_care"), object_name = "05_01_nearcare_by_sex_test", write_log = FALSE)) {
  test_tab <- run_safe_crosstab_test(near_sex_tab, row_var = "sex", col_var = "near_care", n_var = "n")
  ctx <- tc_write_table(
    tab = test_tab,
    table_id = "05_01_nearcare_by_sex_test",
    ctx = ctx,
    output_category = "statistical_test",
    threshold_n_col = "n_total",
    threshold_label_cols = c("test")
  )
}
