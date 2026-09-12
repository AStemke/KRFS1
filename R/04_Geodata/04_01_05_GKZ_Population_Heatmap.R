# Regionale Bevoelkerungs-Heatmap -----------------------------
## Zweck --------------------------
# Erstellt eine externe Referenzkarte der Bevoelkerungszahl je Gemeinde fuer
# dasselbe regionale Gebiet wie die regionale Near-Care-Heatmap.

# Konfiguration -----------------------------
## Kartenparameter --------------------------
heatmap_config <- config$geodata$heatmap
stuttgart_config <- heatmap_config$stuttgart
region_config <- config$analysis$analysis_region
map_gkz_digits <- as.integer(stuttgart_config$gkz_digits)
map_prefix <- region_config$gkz_prefix
map_linewidth <- unname(
  heatmap_config$line_width_by_digits[
    as.character(map_gkz_digits)
  ]
)

# Datenquelle -----------------------------
## Lokale Gemeindedatei --------------------------
population_dir <- file.path(
  config$paths$current_wd,
  "data",
  "Geodata",
  "raw",
  "bevoelkerung"
)

population_files <- if (dir.exists(population_dir)) {
  list.files(
    population_dir,
    pattern = "\\.(xlsx|xls|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
} else {
  character(0)
}

preferred_population_names <- c(
  "bevoelkerung_gemeinden_2023.xlsx",
  "bevoelkerung_gemeinden.xlsx",
  "bevoelkerung_gemeinden_2023.csv",
  "bevoelkerung_gemeinden.csv",
  "GSGG20231201-20231208_G09.xlsx"
)

if (length(population_files) > 0L) {
  preferred_match <- match(
    preferred_population_names,
    basename(population_files),
    nomatch = 0L
  )
  preferred_match <- preferred_match[preferred_match > 0L]
  population_path <- if (length(preferred_match) > 0L) {
    population_files[preferred_match[1L]]
  } else if (length(population_files) == 1L) {
    population_files[1L]
  } else {
    NA_character_
  }
} else {
  population_path <- NA_character_
}
