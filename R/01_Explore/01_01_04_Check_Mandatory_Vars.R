# 01_01_04_Check_Mandatory_Vars
# Zweck: Prüfung auf Existenz und Missingness zentraler Variablen

# Tabelle: Mandatory vars---------------------------

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

ctx <<- tc_write_table(
  tab = mandatory_check,
  table_id = "01_04_check_mandatory_vars",
  ctx = ctx,
  threshold = FALSE
)

## Log-Warnungen---------------------------------------------
if (any(!mandatory_check$exists)) {
  log_write(
    ctx$log_file,
    "WARN",
    "Missing mandatory variables: ",
    paste(mandatory_check$variable[!mandatory_check$exists], collapse = ", ")
  )
}
