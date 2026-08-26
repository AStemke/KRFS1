# 00_01_05_Helper_Stats.R -----------------------------
## Zweck --------------------------
# Datenschutzbewusste Hilfsfunktionen fuer Tests und Modelle.

# Kreuztabellentests -----------------------------
## Sicheren Test aus aggregierter Tabelle ausfuehren --------------------------
run_safe_crosstab_test <- function(tab, row_var, col_var, n_var = "n") {
  stopifnot(all(c(row_var, col_var, n_var) %in% names(tab)))
  wide <- tidyr::pivot_wider(
    tab[, c(row_var, col_var, n_var)],
    names_from = dplyr::all_of(col_var),
    values_from = dplyr::all_of(n_var),
    values_fill = 0
  )
  mat <- as.matrix(wide[, setdiff(names(wide), row_var), drop = FALSE])
  rownames(mat) <- wide[[row_var]]
  
  if (nrow(mat) < 2 || ncol(mat) < 2 || sum(mat) == 0) {
    return(data.frame(
      test = "not_applicable",
      n_total = sum(mat),
      p_value = NA_real_,
      statistic = NA_real_,
      method = "Zu wenige Zeilen/Spalten",
      stringsAsFactors = FALSE
    ))
  }
  
  chi <- suppressWarnings(stats::chisq.test(mat, correct = FALSE))
  if (all(chi$expected >= 5)) {
    return(data.frame(
      test = "chisq",
      n_total = sum(mat),
      p_value = chi$p.value,
      statistic = unname(chi$statistic),
      method = chi$method,
      stringsAsFactors = FALSE
    ))
  }
  
  # Fisher nur fuer kleine Tabellen versuchen, sonst Chi-Quadrat mit Warnhinweis ausgeben.
  if (length(mat) <= 12 && sum(mat) <= 5000) {
    fish <- stats::fisher.test(mat)
    return(data.frame(
      test = "fisher",
      n_total = sum(mat),
      p_value = fish$p.value,
      statistic = NA_real_,
      method = fish$method,
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    test = "chisq_expected_lt5",
    n_total = sum(mat),
    p_value = chi$p.value,
    statistic = unname(chi$statistic),
    method = paste0(chi$method, " (Warnung: erwartete Zellzahlen < 5)"),
    stringsAsFactors = FALSE
  )
}

# Modellpruefungen -----------------------------
## Fallzahlen je Faktorlevel bestimmen --------------------------
check_model_level_counts <- function(data, vars, id_var = "LNGTUMORID") {
  out <- lapply(vars, function(v) {
    if (!v %in% names(data)) return(NULL)
    data |>
      dplyr::filter(!is.na(.data[[v]])) |>
      dplyr::group_by(variable = v, level = as.character(.data[[v]])) |>
      dplyr::summarise(n = dplyr::n_distinct(.data[[id_var]]), .groups = "drop")
  })
  dplyr::bind_rows(out)
}

## Nur Praediktoren mit beobachteter Variation behalten --------------------------
kr_predictors_with_variation <- function(data, vars, min_levels = 2L) {
  vars <- intersect(vars, names(data))
  vars[vapply(
    data[vars],
    function(x) dplyr::n_distinct(x, na.rm = TRUE) >= min_levels,
    logical(1)
  )]
}

# Modelloutputs -----------------------------
## Modellguete fuer glm zusammenfassen --------------------------
kr_glm_fit_stats <- function(fit, model_name) {
  data.frame(
    model = model_name,
    n_model = stats::nobs(fit),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    log_likelihood = as.numeric(stats::logLik(fit)),
    df = attr(stats::logLik(fit), "df"),
    stringsAsFactors = FALSE
  )
}

## Fixed Effects eines glmer-Modells als Odds Ratios ausgeben --------------------------
kr_tidy_glmer_or <- function(fit, model_name) {
  beta <- lme4::fixef(fit)
  se <- sqrt(diag(as.matrix(stats::vcov(fit))))
  z <- beta / se
  p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  
  data.frame(
    model = model_name,
    n_model = stats::nobs(fit),
    estimate_type = "odds_ratio",
    term = names(beta),
    estimate = exp(beta),
    std.error = se,
    statistic = z,
    p.value = p,
    conf.low = exp(beta - stats::qnorm(0.975) * se),
    conf.high = exp(beta + stats::qnorm(0.975) * se),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

## Random-Intercept-Varianz eines glmer-Modells zusammenfassen --------------------------
kr_glmer_random_stats <- function(fit, model_name, group_var) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc <- vc[vc$grp == group_var & is.na(vc$var2), , drop = FALSE]
  
  if (nrow(vc) == 0L) {
    return(data.frame(
      model = model_name,
      n_model = stats::nobs(fit),
      group_var = group_var,
      variance = NA_real_,
      sd = NA_real_,
      icc_latent = NA_real_,
      singular = lme4::isSingular(fit, tol = 1e-4),
      stringsAsFactors = FALSE
    ))
  }
  
  variance <- vc$vcov[1L]
  data.frame(
    model = model_name,
    n_model = stats::nobs(fit),
    group_var = group_var,
    variance = variance,
    sd = sqrt(variance),
    icc_latent = variance / (variance + (pi^2 / 3)),
    singular = lme4::isSingular(fit, tol = 1e-4),
    stringsAsFactors = FALSE
  )
}

# Adjustierte Wahrscheinlichkeiten -----------------------------
## Empirisch standardisierte Wahrscheinlichkeiten fuer Faktorlevel berechnen --------------------------
kr_avg_predictions_glm <- function(fit, data, variable, levels = NULL) {
  stopifnot(inherits(fit, "glm"), is.data.frame(data), variable %in% names(data))
  
  if (is.null(levels)) {
    levels <- unique(as.character(data[[variable]]))
    levels <- levels[!is.na(levels)]
  }
  
  beta <- stats::coef(fit)
  vc <- stats::vcov(fit)
  zcrit <- stats::qnorm(0.975)
  
  out <- lapply(levels, function(level_value) {
    newdata <- data
    
    if (is.factor(data[[variable]])) {
      newdata[[variable]] <- factor(
        rep(level_value, nrow(newdata)),
        levels = levels(data[[variable]])
      )
    } else {
      newdata[[variable]] <- rep(level_value, nrow(newdata))
    }
    
    mm <- stats::model.matrix(stats::delete.response(stats::terms(fit)), newdata)
    mm <- mm[, names(beta), drop = FALSE]
    eta <- as.vector(mm %*% beta)
    prob <- stats::plogis(eta)
    estimate <- mean(prob)
    
    # Delta-Methode fuer den Mittelwert der individuellen Modellwahrscheinlichkeiten.
    gradient <- colMeans(mm * as.vector(prob * (1 - prob)))
    se <- sqrt(as.numeric(t(gradient) %*% vc %*% gradient))
    
    data.frame(
      variable = variable,
      level = as.character(level_value),
      estimate = estimate,
      std.error = se,
      conf.low = pmax(0, estimate - zcrit * se),
      conf.high = pmin(1, estimate + zcrit * se),
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(out)
}
