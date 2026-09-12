# Geodaten-Sourcing -----------------------------
## Zweck --------------------------
# Erstellt die Geodaten-Auswertungen fuer die aktive ICD-10-Kohorte.

# Datenbasis -----------------------------
## Aktive Kohorte --------------------------
tumor_base_all <- if (exists("tumor_base_all")) {
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

# Kontext -----------------------------
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
## Grundauswertungen --------------------------
geodata_dir <- file.path(
  config$paths$current_wd,
  "R",
  "04_Geodata"
)

source(
  file.path(geodata_dir, "04_01_01_GKZ_Diagnosis_Aggregate.R"),
  local = FALSE
)
source(
  file.path(geodata_dir, "04_01_02_GKZ_Choropleth.R"),
  local = FALSE
)

## Heatmaps --------------------------
if (
  config$analysis$active_output_group %in%
    config$geodata$heatmap$output_groups
) {
  if ("bw" %in% config$geodata$heatmap$active_maps) {
    source(
      file.path(geodata_dir, "04_01_03_GKZ_Near_Care_Heatmap.R"),
      local = FALSE
    )
  }

  if ("stuttgart" %in% config$geodata$heatmap$active_maps) {
    source(
      file.path(geodata_dir, "04_01_04_GKZ_Near_Care_Heatmap_Stuttgart.R"),
      local = FALSE
    )
  }
}

## Einwohnerkarte --------------------------
if (
  identical(config$analysis$active_output_group, "Selektiv_Gesamt") &&
    "stuttgart" %in% config$geodata$heatmap$active_maps
) {
  source(
    file.path(geodata_dir, "04_01_05_GKZ_Population_Heatmap.R"),
    local = FALSE
  )
}

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
