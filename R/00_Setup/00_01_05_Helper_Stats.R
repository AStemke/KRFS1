# 00_01_05_Helper_Stats.R -----------
# Zweck:
# - Datenschutzbewusste Hilfsfunktionen fuer Tests und Modelle
# - Tests werden nur geschrieben, wenn die zugrunde liegenden Gruppen/Zellen safe sind

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
    return(data.frame(test = "not_applicable", n_total = sum(mat), p_value = NA_real_, statistic = NA_real_, method = "Zu wenige Zeilen/Spalten", stringsAsFactors = FALSE))
  }
  
  chi <- suppressWarnings(stats::chisq.test(mat, correct = FALSE))
  if (all(chi$expected >= 5)) {
    return(data.frame(test = "chisq", n_total = sum(mat), p_value = chi$p.value, statistic = unname(chi$statistic), method = chi$method, stringsAsFactors = FALSE))
  }
  
  # Fisher nur fuer kleine Tabellen versuchen, sonst Chi-Quadrat mit Warnhinweis ausgeben.
  if (length(mat) <= 12 && sum(mat) <= 5000) {
    fish <- stats::fisher.test(mat)
    return(data.frame(test = "fisher", n_total = sum(mat), p_value = fish$p.value, statistic = NA_real_, method = fish$method, stringsAsFactors = FALSE))
  }
  
  data.frame(test = "chisq_expected_lt5", n_total = sum(mat), p_value = chi$p.value, statistic = unname(chi$statistic), method = paste0(chi$method, " (Warnung: erwartete Zellzahlen < 5)"), stringsAsFactors = FALSE)
}

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
