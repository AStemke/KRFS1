# 01_01-Explore_Sourcing.R

# Init --------------------------------------------------------------------
paths <- explore_init_paths()        # default: config$paths$current_wd
log_file <- explore_log_path(paths)

log_write(log_file, "INFO", "Explore started.")
log_write(log_file, "INFO", "Explore base: ", paths$explore_base)

dat <- ads_list$data
excel_tabs <- list()



# Alle 01_01_ Auswertungen ------------------------------------------------

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "01_Explore"),
  prefix = "01_01_",
  verbose = FALSE,
  local = FALSE
)


# Excel -------------------------------------------------------------------
explore_write_excel(
  excel_list = excel_tabs,
  paths = paths,
  filename = "Explore_Tables.xlsx",
  log_file = log_file
)

log_write(log_file, "INFO", "Explore finished.")
