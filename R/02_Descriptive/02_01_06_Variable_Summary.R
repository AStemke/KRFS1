# Variablenbeschreibung -----------------------------
## Tumorebene --------------------------
tumor_variable_summary <- kr_describe_data(
  data = tumor_base,
  data_level = "Tumorebene"
)

ctx <- tc_write_table(
  tab = tumor_variable_summary,
  table_id = "02_06_tumor_variable_summary",
  ctx = ctx,
  output_category = "descriptive_numeric",
  threshold_n_col = c("n_total", "n_non_missing", "n_missing"),
  threshold_label_cols = c("data_level", "variable", "variable_type", "class")
)

## Patientenebene --------------------------
patient_variable_summary <- kr_describe_data(
  data = patient_base,
  data_level = "Patientenebene"
)

ctx <- tc_write_table(
  tab = patient_variable_summary,
  table_id = "02_07_patient_variable_summary",
  ctx = ctx,
  output_category = "descriptive_numeric",
  threshold_n_col = c("n_total", "n_non_missing", "n_missing"),
  threshold_label_cols = c("data_level", "variable", "variable_type", "class")
)

# Kategoriale Verteilungen -----------------------------
## Tumorebene --------------------------
tumor_category_summary <- kr_describe_categories(
  data = tumor_base,
  data_level = "Tumorebene",
  max_levels = 25L
)

if (nrow(tumor_category_summary) > 0L) {
  ctx <- tc_write_table(
    tab = tumor_category_summary,
    table_id = "02_08_tumor_category_summary",
    ctx = ctx,
    output_category = "crosstab",
    threshold_n_col = "n",
    threshold_label_cols = c("data_level", "variable", "value")
  )
}

## Patientenebene --------------------------
patient_category_summary <- kr_describe_categories(
  data = patient_base,
  data_level = "Patientenebene",
  max_levels = 25L
)

if (nrow(patient_category_summary) > 0L) {
  ctx <- tc_write_table(
    tab = patient_category_summary,
    table_id = "02_09_patient_category_summary",
    ctx = ctx,
    output_category = "crosstab",
    threshold_n_col = "n",
    threshold_label_cols = c("data_level", "variable", "value")
  )
}
