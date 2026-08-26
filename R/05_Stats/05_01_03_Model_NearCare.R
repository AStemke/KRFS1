# Modell wohnortnahe Versorgung -----------------------------
## Analyseziel --------------------------
# Identifikation patienten-, tumor- und regionalbezogener Merkmale,
# die mit einer wohnortnahen Versorgung assoziiert sind.
#
# Primaer werden hierarchische logistische Regressionsmodelle geschaetzt.
# Alter wird nichtlinear ueber natuerliche Splines modelliert.
# Als regionale Erweiterung wird ein Random Intercept fuer den 5-stelligen AGS
# (Landkreis bzw. kreisfreie Stadt) verwendet.

# Modellframe -----------------------------
## Gemeinsame Complete-Case-Basis fuer die Fixed-Effects-Modelle erstellen --------------------------
required_model_vars <- c(
  "LNGTUMORID",
  "distance_km",
  "sex",
  "age_at_diag",
  "uicc",
  "icd10_3",
  "diagnosis_year"
)

missing_model_vars <- setdiff(required_model_vars, names(tumor_base))
if (length(missing_model_vars) > 0L) {
  stop(
    "Fuer das Near-Care-Modell fehlen Variablen in tumor_base: ",
    paste(missing_model_vars, collapse = ", "),
    call. = FALSE
  )
}

model_source_counts <- data.frame(
  step = c("aktive_kohorte", "distanz_verfuegbar", "complete_case_fixed"),
  n = c(
    nrow(tumor_base),
    sum(!is.na(tumor_base$distance_km)),
    sum(stats::complete.cases(tumor_base[, required_model_vars, drop = FALSE]))
  ),
  stringsAsFactors = FALSE
)

model_dat <- tumor_base |>
  dplyr::filter(
    !is.na(distance_km),
    !is.na(sex),
    !is.na(age_at_diag),
    !is.na(uicc),
    !is.na(icd10_3),
    !is.na(diagnosis_year)
  ) |>
  dplyr::mutate(
    near_care_num = as.integer(distance_km <= config$analysis$near_care_km),
    sex = as.factor(sex),
    uicc = as.factor(uicc),
    icd10_3 = as.factor(icd10_3),
    diagnosis_year_c = diagnosis_year - mean(diagnosis_year, na.rm = TRUE),
    gkz_5 = dplyr::if_else(
      !is.na(gkz_diagnose) & nchar(gkz_diagnose) >= 5L,
      substr(gkz_diagnose, 1L, 5L),
      NA_character_
    )
  )

ctx <- tc_write_table(
  tab = model_source_counts,
  table_id = "05_03_nearcare_model_source_counts",
  ctx = ctx,
  output_category = "model_result",
  threshold_n_col = "n",
  threshold_label_cols = "step"
)

# Modellvoraussetzungen -----------------------------
## Nutzbare Praediktoren bestimmen --------------------------
categorical_candidates <- c("sex", "uicc", "icd10_3")
categorical_predictors <- kr_predictors_with_variation(
  data = model_dat,
  vars = categorical_candidates
)

age_has_variation <- dplyr::n_distinct(model_dat$age_at_diag, na.rm = TRUE) >= 4L
year_has_variation <- dplyr::n_distinct(model_dat$diagnosis_year_c, na.rm = TRUE) >= 2L

omitted_predictors <- c(
  setdiff(categorical_candidates, categorical_predictors),
  if (!age_has_variation) "age_at_diag" else character(0),
  if (!year_has_variation) "diagnosis_year" else character(0)
)

if (length(omitted_predictors) > 0L) {
  log_write(
    ctx$log_file,
    "WARN",
    "Near-Care-Modell: Praediktoren ohne ausreichende Variation ausgelassen: ",
    paste(omitted_predictors, collapse = ", "),
    "."
  )
}

level_counts <- check_model_level_counts(
  data = model_dat,
  vars = categorical_predictors,
  id_var = "LNGTUMORID"
)

if (nrow(level_counts) > 0L) {
  ctx <- tc_write_table(
    tab = level_counts,
    table_id = "05_03_nearcare_model_level_counts",
    ctx = ctx,
    output_category = "model_result",
    threshold_n_col = "n",
    threshold_label_cols = c("variable", "level")
  )
}

# Modellstruktur -----------------------------
## Hierarchische Fixed-Effects-Modelle definieren --------------------------
patient_terms <- character(0)
if ("sex" %in% categorical_predictors) {
  patient_terms <- c(patient_terms, "sex")
}
if (age_has_variation) {
  patient_terms <- c(patient_terms, "splines::ns(age_at_diag, df = 3)")
}

tumor_terms <- c(patient_terms, intersect(c("uicc", "icd10_3"), categorical_predictors))
full_terms <- tumor_terms
if (year_has_variation) {
  full_terms <- c(full_terms, "diagnosis_year_c")
}

if (length(patient_terms) == 0L) {
  patient_terms <- "1"
}
if (length(tumor_terms) == 0L) {
  tumor_terms <- "1"
}
if (length(full_terms) == 0L) {
  full_terms <- "1"
}

formula_patient <- stats::as.formula(
  paste("near_care_num ~", paste(patient_terms, collapse = " + "))
)
formula_tumor <- stats::as.formula(
  paste("near_care_num ~", paste(tumor_terms, collapse = " + "))
)
formula_full <- stats::as.formula(
  paste("near_care_num ~", paste(full_terms, collapse = " + "))
)

model_spec <- data.frame(
  model = c("M1_patient", "M2_patient_tumor", "M3_full_fixed"),
  formula = c(
    paste(deparse(formula_patient), collapse = " "),
    paste(deparse(formula_tumor), collapse = " "),
    paste(deparse(formula_full), collapse = " ")
  ),
  stringsAsFactors = FALSE
)

ctx <- tc_write_table(
  tab = model_spec,
  table_id = "05_03_nearcare_model_specification",
  ctx = ctx,
  output_category = "technical_overview",
  threshold = FALSE
)

# Modellschaetzung -----------------------------
## Datenschutz und Outcome-Variation pruefen --------------------------
model_counts <- data.frame(
  model = "fixed_model_frame",
  n_model = nrow(model_dat),
  n_events = sum(model_dat$near_care_num == 1L, na.rm = TRUE),
  n_nonevents = sum(model_dat$near_care_num == 0L, na.rm = TRUE),
  stringsAsFactors = FALSE
)

ctx <- tc_write_table(
  tab = model_counts,
  table_id = "05_03_nearcare_model_counts",
  ctx = ctx,
  output_category = "model_result",
  threshold_n_col = c("n_model", "n_events", "n_nonevents"),
  threshold_label_cols = "model"
)

fixed_levels_safe <- if (nrow(level_counts) == 0L) {
  TRUE
} else {
  passes_threshold_category(
    level_counts,
    "model_result",
    n_col = "n",
    label_cols = c("variable", "level"),
    object_name = "05_03_nearcare_model_levels",
    write_log = FALSE
  )
}

ok_fixed_model <-
  passes_threshold_category(
    model_counts,
    "model_result",
    n_col = c("n_model", "n_events", "n_nonevents"),
    label_cols = "model",
    object_name = "05_03_nearcare_model_counts",
    write_log = FALSE
  ) &&
  fixed_levels_safe &&
  nrow(model_dat) > 0L &&
  dplyr::n_distinct(model_dat$near_care_num) == 2L

## Hierarchische logistische Regressionen ausfuehren --------------------------
if (isTRUE(ok_fixed_model)) {
  fit_m1 <- stats::glm(
    formula_patient,
    data = model_dat,
    family = stats::binomial()
  )
  fit_m2 <- stats::glm(
    formula_tumor,
    data = model_dat,
    family = stats::binomial()
  )
  fit_m3 <- stats::glm(
    formula_full,
    data = model_dat,
    family = stats::binomial()
  )
  
  # Odds Ratios fuer alle drei Modellstufen.
  fit_or <- dplyr::bind_rows(
    broom::tidy(fit_m1, conf.int = TRUE, exponentiate = TRUE) |>
      dplyr::mutate(model = "M1_patient", n_model = nrow(model_dat)),
    broom::tidy(fit_m2, conf.int = TRUE, exponentiate = TRUE) |>
      dplyr::mutate(model = "M2_patient_tumor", n_model = nrow(model_dat)),
    broom::tidy(fit_m3, conf.int = TRUE, exponentiate = TRUE) |>
      dplyr::mutate(model = "M3_full_fixed", n_model = nrow(model_dat))
  ) |>
    dplyr::mutate(estimate_type = "odds_ratio") |>
    dplyr::select(model, n_model, estimate_type, dplyr::everything())
  
  ctx <- tc_write_table(
    tab = fit_or,
    table_id = "05_03_nearcare_model_or",
    ctx = ctx,
    output_category = "model_result",
    threshold_n_col = "n_model",
    threshold_label_cols = c("model", "term")
  )
  
  # AIC/BIC und Log-Likelihood fuer die hierarchischen Modelle.
  fit_stats <- dplyr::bind_rows(
    kr_glm_fit_stats(fit_m1, "M1_patient"),
    kr_glm_fit_stats(fit_m2, "M2_patient_tumor"),
    kr_glm_fit_stats(fit_m3, "M3_full_fixed")
  )
  
  ctx <- tc_write_table(
    tab = fit_stats,
    table_id = "05_03_nearcare_model_fit",
    ctx = ctx,
    output_category = "model_result",
    threshold_n_col = "n_model",
    threshold_label_cols = "model"
  )
  
  # Likelihood-Ratio-Vergleich der Modellstufen auf identischem Modellframe.
  model_comparison <- dplyr::bind_rows(
    broom::tidy(stats::anova(fit_m1, fit_m2, test = "Chisq")) |>
      dplyr::mutate(comparison = "M1_vs_M2"),
    broom::tidy(stats::anova(fit_m2, fit_m3, test = "Chisq")) |>
      dplyr::mutate(comparison = "M2_vs_M3")
  ) |>
    dplyr::mutate(n_model = nrow(model_dat)) |>
    dplyr::select(comparison, n_model, dplyr::everything())
  
  ctx <- tc_write_table(
    tab = model_comparison,
    table_id = "05_03_nearcare_model_comparison",
    ctx = ctx,
    output_category = "model_result",
    threshold_n_col = "n_model",
    threshold_label_cols = "comparison"
  )
  
  # Adjustierte Wahrscheinlichkeiten aus dem voll adjustierten Fixed-Effects-Modell.
  prediction_vars <- intersect(c("sex", "uicc", "icd10_3"), categorical_predictors)
  adjusted_probabilities <- dplyr::bind_rows(lapply(prediction_vars, function(v) {
    pred <- kr_avg_predictions_glm(
      fit = fit_m3,
      data = model_dat,
      variable = v
    )
    counts_v <- level_counts |>
      dplyr::filter(variable == v) |>
      dplyr::select(variable, level, n)
    dplyr::left_join(pred, counts_v, by = c("variable", "level"))
  }))
  
  if (nrow(adjusted_probabilities) > 0L) {
    ctx <- tc_write_table(
      tab = adjusted_probabilities,
      table_id = "05_03_nearcare_adjusted_probabilities",
      ctx = ctx,
      output_category = "model_result",
      threshold_n_col = "n",
      threshold_label_cols = c("variable", "level")
    )
    
    adjusted_probability_plot <- ggplot2::ggplot(
      adjusted_probabilities,
      ggplot2::aes(
        x = level,
        y = estimate,
        ymin = conf.low,
        ymax = conf.high
      )
    ) +
      ggplot2::geom_pointrange() +
      ggplot2::facet_wrap(~ variable, scales = "free_x") +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      ggplot2::labs(
        title = "Adjustierte Wahrscheinlichkeit wohnortnaher Versorgung",
        x = NULL,
        y = "Adjustierte Wahrscheinlichkeit"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ctx <- tc_write_figure(
      plot = adjusted_probability_plot,
      fig_id = "05_03",
      filename = "nearcare_adjusted_probabilities",
      ctx = ctx,
      threshold_tab = adjusted_probabilities,
      output_category = "model_result",
      threshold_n_col = "n",
      threshold_label_cols = c("variable", "level")
    )
  }
  
  # Forest Plot fuer direkt interpretierbare Koeffizienten des voll adjustierten Modells.
  forest_dat <- fit_or |>
    dplyr::filter(
      model == "M3_full_fixed",
      term != "(Intercept)",
      !grepl("^splines::ns\\(age_at_diag", term)
    )
  
  if (nrow(forest_dat) > 0L) {
    forest_plot <- ggplot2::ggplot(
      forest_dat,
      ggplot2::aes(
        x = stats::reorder(term, estimate),
        y = estimate,
        ymin = conf.low,
        ymax = conf.high
      )
    ) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::geom_pointrange() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        title = "Assoziationen mit wohnortnaher Versorgung",
        x = NULL,
        y = "Odds Ratio mit 95%-KI"
      ) +
      ggplot2::theme_minimal()
    
    ctx <- tc_write_figure(
      plot = forest_plot,
      fig_id = "05_03",
      filename = "nearcare_forest_plot",
      ctx = ctx,
      threshold_tab = model_counts,
      output_category = "model_result",
      threshold_n_col = c("n_model", "n_events", "n_nonevents"),
      threshold_label_cols = "model"
    )
  }
} else {
  ctx <- tc_add_suppressed(
    ctx,
    object_id = "05_03_nearcare_fixed_models",
    output_category = "model_result",
    reason = "Fixed-Effects-Modelle nicht ausgegeben: Modellframe, Events oder Faktorlevel nicht safe bzw. Outcome ohne Variation."
  )
}

# Regionales Mixed-Effects-Modell -----------------------------
## Regionale Modellbasis mit 5-stelligem AGS erstellen --------------------------
if (isTRUE(ok_fixed_model)) {
  model_dat_region <- model_dat |>
    dplyr::filter(!is.na(gkz_5)) |>
    dplyr::mutate(gkz_5 = as.factor(gkz_5))
  
  region_counts <- data.frame(
    model = "M4_region_random_intercept",
    n_model = nrow(model_dat_region),
    n_events = sum(model_dat_region$near_care_num == 1L, na.rm = TRUE),
    n_nonevents = sum(model_dat_region$near_care_num == 0L, na.rm = TRUE),
    n_regions = dplyr::n_distinct(model_dat_region$gkz_5),
    stringsAsFactors = FALSE
  )
  
  ctx <- tc_write_table(
    tab = region_counts,
    table_id = "05_03_nearcare_mixed_model_counts",
    ctx = ctx,
    output_category = "model_result",
    threshold_n_col = c("n_model", "n_events", "n_nonevents"),
    threshold_label_cols = "model"
  )
  
  ok_region_model <-
    passes_threshold_category(
      region_counts,
      "model_result",
      n_col = c("n_model", "n_events", "n_nonevents"),
      label_cols = "model",
      object_name = "05_03_nearcare_mixed_model_counts",
      write_log = FALSE
    ) &&
    nrow(model_dat_region) > 0L &&
    dplyr::n_distinct(model_dat_region$near_care_num) == 2L &&
    dplyr::n_distinct(model_dat_region$gkz_5) >= 2L
  
  if (isTRUE(ok_region_model)) {
    formula_mixed <- stats::as.formula(
      paste0(
        "near_care_num ~ ",
        paste(full_terms, collapse = " + "),
        " + (1 | gkz_5)"
      )
    )
    
    fit_m4 <- lme4::glmer(
      formula_mixed,
      data = model_dat_region,
      family = stats::binomial(),
      control = lme4::glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    )
    
    mixed_or <- kr_tidy_glmer_or(
      fit = fit_m4,
      model_name = "M4_region_random_intercept"
    )
    
    ctx <- tc_write_table(
      tab = mixed_or,
      table_id = "05_03_nearcare_mixed_model_or",
      ctx = ctx,
      output_category = "model_result",
      threshold_n_col = "n_model",
      threshold_label_cols = c("model", "term")
    )
    
    random_stats <- kr_glmer_random_stats(
      fit = fit_m4,
      model_name = "M4_region_random_intercept",
      group_var = "gkz_5"
    ) |>
      dplyr::mutate(n_regions = dplyr::n_distinct(model_dat_region$gkz_5))
    
    ctx <- tc_write_table(
      tab = random_stats,
      table_id = "05_03_nearcare_mixed_model_region",
      ctx = ctx,
      output_category = "model_result",
      threshold_n_col = "n_model",
      threshold_label_cols = "model"
    )
    
    if (isTRUE(lme4::isSingular(fit_m4, tol = 1e-4))) {
      log_write(
        ctx$log_file,
        "WARN",
        "Near-Care Mixed Model ist singulaer; Random-Intercept-Varianz liegt nahe 0."
      )
    }
  } else {
    ctx <- tc_add_suppressed(
      ctx,
      object_id = "05_03_nearcare_mixed_model",
      output_category = "model_result",
      reason = "Mixed-Effects-Modell nicht ausgegeben: zu wenige sichere Faelle/Events oder weniger als zwei Regionen."
    )
  }
}
