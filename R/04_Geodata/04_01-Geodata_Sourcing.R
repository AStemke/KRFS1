# 04_01-Geodata_Sourcing.R -----------
# Zweck:
# - Aggregierte Karten- und Gebietsauswertungen vorbereiten
# - Ohne vorhandene Geodaten werden Tabellen erzeugt, Karten aber uebersprungen

ctx <- tc_init_ctx(scope = "Geodata")

tumor_base <- if (exists("tumor_base")) tumor_base else readRDS(file.path(config$paths$current_wd, "output", "Prepare", "rds", "tumor_base.rds"))

gkz_geo <- NULL
if (isTRUE(config$geodata$use_geodata)) {
  gkz_geo <- load_sf_optional(
    path = file.path(config$paths$current_wd, config$geodata$gkz_path),
    layer = config$geodata$gkz_layer,
    crs_target = config$geodata$crs_target,
    log_file = ctx$log_file
  )
}

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "04_Geodata"),
  prefix = "04_01_",
  verbose = FALSE,
  local = FALSE
)

tc_write_excel(ctx, filename = "Geodata_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Geodata finished.")
