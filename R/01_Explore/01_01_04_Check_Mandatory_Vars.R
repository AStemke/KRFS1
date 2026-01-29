# 01_01_04_Check_Mandatory_Vars.R


mandatory_check <- dplyr::tibble(variable = config$mandatory_vars) %>%
  dplyr::mutate(
    exists = variable %in% names(dat),
    n_na = dplyr::if_else(
      exists,
      vapply(variable, function(v) sum(is.na(dat[[v]])), integer(1)),
      NA_integer_
    ),
    pct_na = dplyr::if_else(exists, n_na / nrow(dat), NA_real_)
  )

excel_tabs <<- explore_write_table(
  tab = mandatory_check,
  table_id = "01_04_check_mandatory_vars",
  paths = paths,
  excel_list = excel_tabs,
  log_file = log_file
)

missing_vars <- mandatory_check %>% dplyr::filter(!exists) %>% dplyr::pull(variable)
if (length(missing_vars) > 0) {
  log_write(log_file, "WARN", "Mandatory vars missing: ", paste(missing_vars, collapse = ", "))
}

mandatory_with_na <- mandatory_check %>%
  dplyr::filter(exists, !is.na(pct_na), pct_na > 0)

if (nrow(mandatory_with_na) > 0) {
  for (i in seq_len(nrow(mandatory_with_na))) {
    log_write(
      log_file,
      "WARN",
      "Mandatory var has NA: ", mandatory_with_na$variable[i],
      " (pct_na=", round(100 * mandatory_with_na$pct_na[i], 2), "%)"
    )
  }
}
