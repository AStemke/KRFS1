# 05_01_03_Model_NearCare.R -----------
# Zweck: Beispielmodell fuer wohnortnahe Versorgung
# Modell: near_care ~ sex + age_group + uicc
# Datenschutz: Modellframe und alle Faktorlevel muessen safe sein

model_dat <- tumor_base |>
  dplyr::filter(!is.na(near_care), !is.na(sex), !is.na(age_group), !is.na(uicc)) |>
  dplyr::mutate(
    near_care_num = as.integer(near_care),
    age_group = factor(as.character(age_group), levels = config$explore$age_labels),
    uicc = as.factor(uicc),
    sex = as.factor(sex)
  )

model_counts <- data.frame(
  model = "nearcare_logistic",
  n_model = nrow(model_dat),
  n_events = sum(model_dat$near_care_num == 1, na.rm = TRUE),
  n_nonevents = sum(model_dat$near_care_num == 0, na.rm = TRUE),
  stringsAsFactors = FALSE
)

level_counts <- check_model_level_counts(
  data = model_dat,
  vars = c("sex", "age_group", "uicc"),
  id_var = "LNGTUMORID"
)

ctx <- tc_write_table(
  tab = model_counts,
  table_id = "05_03_nearcare_model_counts",
  ctx = ctx,
  output_category = "model_result",
  threshold_n_col = c("n_model", "n_events", "n_nonevents"),
  threshold_label_cols = c("model")
)

ctx <- tc_write_table(
  tab = level_counts,
  table_id = "05_03_nearcare_model_level_counts",
  ctx = ctx,
  output_category = "model_result",
  threshold_n_col = "n",
  threshold_label_cols = c("variable", "level")
)

ok_model <- passes_threshold_category(model_counts, "model_result", n_col = c("n_model", "n_events", "n_nonevents"), label_cols = c("model"), object_name = "05_03_nearcare_model_counts", write_log = FALSE) &&
  passes_threshold_category(level_counts, "model_result", n_col = "n", label_cols = c("variable", "level"), object_name = "05_03_nearcare_model_levels", write_log = FALSE) &&
  nrow(model_dat) > 0 && dplyr::n_distinct(model_dat$near_care_num) == 2

if (isTRUE(ok_model)) {
  fit <- stats::glm(near_care_num ~ sex + age_group + uicc, data = model_dat, family = stats::binomial())
  fit_tab <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) |>
    dplyr::mutate(
      model = "nearcare_logistic",
      n_model = nrow(model_dat),
      estimate_type = "odds_ratio"
    ) |>
    dplyr::select(model, n_model, estimate_type, dplyr::everything())
  
  ctx <- tc_write_table(
    tab = fit_tab,
    table_id = "05_03_nearcare_model_or",
    ctx = ctx,
    output_category = "model_result",
    threshold_n_col = "n_model",
    threshold_label_cols = c("model", "term")
  )
} else {
  ctx <- tc_add_suppressed(
    ctx,
    object_id = "05_03_nearcare_model_or",
    output_category = "model_result",
    reason = "Modell nicht ausgegeben: Modellframe/Events/Level nicht safe oder Outcome hat nur eine Auspraegung."
  )
}
