# 04_01-Geodata_Sourcing.R
#
# Erstellt die Geodaten-Auswertungen fuer die aktive ICD-10-Kohorte.
# Tabellen und das Gebietsdiagramm werden auch ohne Geometrien erzeugt;
# ausschließlich die Karten werden bei fehlenden Geodaten uebersprungen.


# Datenbasis -----------------------------
## Aktive Kohorte --------------------------

tumor_base_all <- if (
  exists("tumor_base_all")
) {
  tumor_base_all
} else {
  readRDS(
    file.path(
      config$paths$current_wd,
      "output",
      "Gesamt",
      "Prepare",
      "rds",
      "tumor_base.rds"
    )
  )
}

tumor_base <- kr_filter_tumor_cohort(
  tumor_base = tumor_base_all,
  icd10_3_keep = config$analysis$active_icd10_3
)


## Ausgabekontext --------------------------

ctx <- tc_init_ctx(
  scope = file.path(
    config$analysis$active_output_group,
    "Geodata"
  )
)


# Geodaten -----------------------------
## GKZ-Geometrien --------------------------

gkz_geo <- NULL

if (isTRUE(config$geodata$use_geodata)) {
  gkz_geo_path <- resolve_geodata_path(
    path = config$geodata$gkz_path,
    current_wd = config$paths$current_wd
  )

  gkz_geo <- load_sf_optional(
    path = gkz_geo_path,
    layer = config$geodata$gkz_layer,
    crs_target = config$geodata$crs_target,
    log_file = ctx$log_file
  )
}


# Auswertung -----------------------------
## Analyseskripte --------------------------

source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path(
    "R",
    "04_Geodata"
  ),
  prefix = "04_01_",
  verbose = FALSE,
  local = FALSE
)


# Ergebnisse -----------------------------
## Tabellenmappe und Log --------------------------

tc_write_excel(
  ctx,
  filename = "Geodata_Tables.xlsx"
)

log_write(
  ctx$log_file,
  "INFO",
  "Geodata finished for ",
  config$analysis$active_output_group,
  "."
)
