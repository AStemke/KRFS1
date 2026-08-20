# 01_01_02_Check_Mandatory_Vars.R -----------
# Zweck:
# - Existenz und Missingness zentraler Variablen pruefen

mandatory_check <- data.frame(variable = config$mandatory_vars, stringsAsFactors = FALSE) |>
  dplyr::mutate(
    exists = variable %in% names(dat_raw),
    n_na = vapply(variable, function(v) {
      if (!v %in% names(dat_raw)) return(NA_integer_)
      sum(is.na(dat_raw[[v]]) | dat_raw[[v]] == "")
    }, integer(1)),
    pct_na = safe_divide(n_na, nrow(dat_raw))
  )

ctx <- tc_write_table(
  tab = mandatory_check,
  table_id = "01_02_mandatory_vars",
  ctx = ctx,
  output_category = "technical_overview",
  threshold = FALSE
)

if (any(!mandatory_check$exists)) {
  log_write(
    ctx$log_file,
    "WARN",
    "Missing mandatory variables: ",
    paste(mandatory_check$variable[!mandatory_check$exists], collapse = ", ")
  )
}
