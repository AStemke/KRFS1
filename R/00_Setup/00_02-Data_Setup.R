# 00_02-Data_Setup.R -----------
# Zweck:
# - Laedt Daten-Helfer und erzeugt ads_list

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "00_Setup"),
  prefix = "00_02_",
  verbose = FALSE,
  local = FALSE
)

ads_list <- load_ads(
  current_wd = config$paths$current_wd,
  productive = config$data$productive
)
