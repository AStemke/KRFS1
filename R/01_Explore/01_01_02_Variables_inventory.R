# 01_01_02_Variables_inventory.R

variables_inventory <- dplyr::tibble(variable = names(dat)) %>%
  dplyr::mutate(
    type       = vapply(dat, function(x) paste(class(x), collapse = "/"), character(1)),
    n          = nrow(dat),
    n_na       = vapply(dat, function(x) sum(is.na(x)), integer(1)),
    pct_na     = n_na / n,
    n_distinct = vapply(dat, function(x) dplyr::n_distinct(x), integer(1))
  ) %>%
  dplyr::arrange(dplyr::desc(pct_na), variable)

excel_tabs <<- explore_write_table(
  tab = variables_inventory,
  table_id = "01_02_variables_inventory",
  paths = paths,
  excel_list = excel_tabs,
  log_file = log_file
)
