# 01_01-Prepare_Sourcing.R -----------------------------
## Zweck --------------------------
# Erstellt Meldungs-, Tumor- und Patientenbasis aus Rohdaten.
# Die unselektierte Basis wird unter output/Gesamt/Prepare gespeichert.

# Kontext -----------------------------
## Gesamtpopulation vorbereiten --------------------------
config$analysis$active_output_group <- "Gesamt"
config$analysis$active_icd10_3 <- character(0)

ctx <- tc_init_ctx(scope = file.path("Gesamt", "Prepare"))
dat_raw <- ads_list$data
legend <- ads_list$legend

# Vorbereitung -----------------------------
## Prepare-Skripte ausfuehren --------------------------
source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "01_Prepare"),
  prefix = "01_01_",
  verbose = FALSE,
  local = FALSE
)

# Output -----------------------------
## Tabellenmappe schreiben --------------------------
tc_write_excel(ctx, filename = "Prepare_Tables.xlsx")
log_write(ctx$log_file, "INFO", "Prepare finished.")
