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

# Hilfsfunktionen -----------------------------
## Import und Spaltenerkennung --------------------------
kr_population_clean_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}

kr_population_read_excel <- function(path) {
  sheets <- readxl::excel_sheets(path)

  for (sheet in sheets) {
    preview <- suppressMessages(
      readxl::read_excel(
        path,
        sheet = sheet,
        col_names = FALSE,
        n_max = 50,
        .name_repair = "minimal"
      )
    )

    if (nrow(preview) == 0L) {
      next
    }

    row_text <- apply(
      preview,
      1L,
      function(x) paste(as.character(x), collapse = " ")
    )

    header_rows <- which(
      grepl(
        "gs.*schl|gemeinde.*schl|ags|gkz",
        row_text,
        ignore.case = TRUE
      ) &
        grepl(
          "bevoelker|einwohner|population",
          row_text,
          ignore.case = TRUE
        )
    )

    if (length(header_rows) > 0L) {
      return(
        suppressMessages(
          readxl::read_excel(
            path,
            sheet = sheet,
            skip = header_rows[1L] - 1L,
            .name_repair = "unique"
          )
        )
      )
    }
  }

  suppressMessages(
    readxl::read_excel(
      path,
      sheet = sheets[1L],
      .name_repair = "unique"
    )
  )
}

kr_population_read_csv <- function(path) {
  first_line <- readLines(
    path,
    n = 1L,
    warn = FALSE,
    encoding = "UTF-8"
  )

  n_semicolon <- stringr::str_count(first_line, ";")
  n_comma <- stringr::str_count(first_line, ",")
  delimiter <- if (n_semicolon >= n_comma) ";" else ","

  suppressMessages(
    readr::read_delim(
      path,
      delim = delimiter,
      show_col_types = FALSE,
      trim_ws = TRUE,
      name_repair = "unique"
    )
  )
}
