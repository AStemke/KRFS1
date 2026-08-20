# 03_01-Access_Sourcing.R -----------
# Zweck:
# - Auswertungen zur wohnortnahen Versorgung anhand Distanz_km

ctx <- tc_init_ctx(scope = "Access")

tumor_base <- if (exists("tumor_base")) tumor_base else readRDS(file.path(config$paths$current_wd, "output", "Prepare", "rds", "tumor_base.rds"))

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "03_Access"),
  prefix = "03_01_",
  verbose = FALSE,
  local = FALSE
)

tc_write_excel(ctx, filename = "Access_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Access finished.")
