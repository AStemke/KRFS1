# 02_01-Descriptive_Sourcing.R -----------
# Zweck:
# - Deskriptive Standardauswertungen auf Tumorebene

ctx <- tc_init_ctx(scope = "Descriptive")

tumor_base <- if (exists("tumor_base")) tumor_base else readRDS(file.path(config$paths$current_wd, "output", "Prepare", "rds", "tumor_base.rds"))
patient_base <- if (exists("patient_base")) patient_base else readRDS(file.path(config$paths$current_wd, "output", "Prepare", "rds", "patient_base.rds"))

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "02_Descriptive"),
  prefix = "02_01_",
  verbose = FALSE,
  local = FALSE
)

tc_write_excel(ctx, filename = "Descriptive_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Descriptive finished.")
