# 04_01-Geodata_Sourcing.R -----------------------------
## Zweck --------------------------
# Aggregierte Karten- und Gebietsauswertungen fuer die aktive ICD-10-Kohorte vorbereiten.
# Ohne vorhandene Geodaten werden Tabellen erzeugt, Karten aber uebersprungen.

# Datenbasis -----------------------------
## Gesamtbasis laden und aktive Kohorte filtern --------------------------
tumor_base_all <- if (exists("tumor_base_all")) {
  tumor_base_all
} else {
  readRDS(file.path(config$paths$current_wd, "output", "Gesamt", "Prepare", "rds", "tumor_base.rds"))
}

tumor_base <- kr_filter_tumor_cohort(
  tumor_base = tumor_base_all,
  icd10_3_keep = config$analysis$active_icd10_3
)

# Kontext -----------------------------
## Kohortenspezifischen Output initialisieren --------------------------
ctx <- tc_init_ctx(scope = file.path(config$analysis$active_output_group, "Geodata"))

# Geodaten -----------------------------
## GKZ-Geometrien laden --------------------------
gkz_geo <- NULL
if (isTRUE(config$geodata$use_geodata)) {
  gkz_geo <- load_sf_optional(
    path = file.path(config$paths$current_wd, config$geodata$gkz_path),
    layer = config$geodata$gkz_layer,
    crs_target = config$geodata$crs_target,
    log_file = ctx$log_file
  )
}

# Auswertung -----------------------------
## Geodata-Skripte ausfuehren --------------------------
source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "04_Geodata"),
  prefix = "04_01_",
  verbose = FALSE,
  local = FALSE
)

# Output -----------------------------
## Tabellenmappe schreiben --------------------------
tc_write_excel(ctx, filename = "Geodata_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Geodata finished for ", config$analysis$active_output_group, ".")
