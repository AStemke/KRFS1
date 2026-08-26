# 03_01-Access_Sourcing.R -----------------------------
## Zweck --------------------------
# Auswertungen zur wohnortnahen Versorgung anhand Distanz_km fuer die aktive ICD-10-Kohorte.

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
ctx <- tc_init_ctx(scope = file.path(config$analysis$active_output_group, "Access"))

# Auswertung -----------------------------
## Access-Skripte ausfuehren --------------------------
source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "03_Access"),
  prefix = "03_01_",
  verbose = FALSE,
  local = FALSE
)

# Output -----------------------------
## Tabellenmappe schreiben --------------------------
tc_write_excel(ctx, filename = "Access_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Access finished for ", config$analysis$active_output_group, ".")
