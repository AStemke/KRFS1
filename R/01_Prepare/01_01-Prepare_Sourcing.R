# 01_01-Prepare_Sourcing.R -----------
# Zweck:
# - Erstellt Meldungs-, Tumor- und Patientenbasis aus Rohdaten

ctx <- tc_init_ctx(scope = "Prepare")
dat_raw <- ads_list$data
legend <- ads_list$legend

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "01_Prepare"),
  prefix = "01_01_",
  verbose = FALSE,
  local = FALSE
)

tc_write_excel(ctx, filename = "Prepare_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Prepare finished.")
