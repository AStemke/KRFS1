# 01_01-Explore_Sourcing.R

# Init --------------------------------------------------------------
paths <- tc_init_paths(scope = "Explore")
log_file <- tc_log_path(paths)

log_write(log_file, "INFO", "Started scope: ", paths$scope)
log_write(log_file, "INFO", "Output base: ", paths$output_base)

# Daten --------------------------------------------------------------
dat <- ads_list$data
ctx <- list(paths = paths, log_file = log_file, excel_tabs = list())

# Alle 01_01_ Auswertungen ------------------------------------------------
source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "01_Explore"),
  prefix = "01_01_",
  verbose = FALSE,
  local = FALSE
)

# Excel -------------------------------------------------------------------
tc_write_excel(
  excel_list = ctx$excel_tabs,
  paths = ctx$paths,
  filename = "Explore_Tables.xlsx",
  log_file = ctx$log_file
)

log_write(log_file, "INFO", "Explore finished.")
