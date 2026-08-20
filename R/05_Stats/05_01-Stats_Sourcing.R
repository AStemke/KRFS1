# 05_01-Stats_Sourcing.R -----------
# Zweck:
# - Datenschutzbewusste Tests und erste Modelle

ctx <- tc_init_ctx(scope = "Stats")

tumor_base <- if (exists("tumor_base")) tumor_base else readRDS(file.path(config$paths$current_wd, "output", "Prepare", "rds", "tumor_base.rds"))

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "05_Stats"),
  prefix = "05_01_",
  verbose = FALSE,
  local = FALSE
)

tc_write_excel(ctx, filename = "Stats_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Stats finished.")
