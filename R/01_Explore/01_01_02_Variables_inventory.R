# 01_01_02_Variables_inventory
# Zweck: Überblick über Variablen, Typen und Missingness

# Tabelle: Variables inventory---------------------------
variables_inventory <- dplyr::tibble(variable = names(dat)) %>%
  dplyr::mutate(
    type       = vapply(dat, function(x) paste(class(x), collapse = "/"), character(1)),
    n          = nrow(dat),
    n_na       = vapply(dat, function(x) sum(is.na(x)), integer(1)),
    pct_na     = n_na / n,
    n_distinct = vapply(dat, dplyr::n_distinct, integer(1))
  ) %>%
  dplyr::arrange(dplyr::desc(pct_na), variable)

ctx <<- tc_write_table(
  tab = variables_inventory,
  table_id = "01_02_variables_inventory",
  ctx = ctx,
  threshold = FALSE
)
