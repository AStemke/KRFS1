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

# Verarbeitung -----------------------------
## Datei pruefen --------------------------
if (is.na(population_path)) {
  population_status <- data.frame(
    object = "04_05_gkz_population_heatmap",
    status = "uebersprungen",
    reason = paste0(
      "Keine eindeutige Bevoelkerungsdatei in ",
      population_dir,
      " gefunden."
    ),
    analysis_group = config$analysis$active_output_group,
    stringsAsFactors = FALSE
  )

  ctx <- tc_write_table(
    tab = population_status,
    table_id = "04_05_population_heatmap_status",
    ctx = ctx,
    output_category = "technical_overview",
    threshold = FALSE
  )
} else if (
  !exists("gkz_geo") ||
    is.null(gkz_geo) ||
    !inherits(gkz_geo, "sf")
) {
  population_status <- data.frame(
    object = "04_05_gkz_population_heatmap",
    status = "uebersprungen",
    reason = "Keine GKZ-Geodaten vorhanden",
    analysis_group = config$analysis$active_output_group,
    stringsAsFactors = FALSE
  )

  ctx <- tc_write_table(
    tab = population_status,
    table_id = "04_05_population_heatmap_status",
    ctx = ctx,
    output_category = "technical_overview",
    threshold = FALSE
  )
} else {
  # Datenaufbereitung -----------------------------
  ## Gemeindedaten einlesen --------------------------
  population_extension <- tolower(tools::file_ext(population_path))

  population_raw <- if (population_extension %in% c("xlsx", "xls")) {
    kr_population_read_excel(population_path)
  } else {
    kr_population_read_csv(population_path)
  }

  population_name_key <- kr_population_clean_name(names(population_raw))

  # Bei Exporten mit mehreren Stichtagen wird 31.12.2023 bevorzugt.
  date_col <- grep(
    "stichtag|datum|date",
    population_name_key
  )[1L]

  if (!is.na(date_col)) {
    date_values <- as.character(population_raw[[date_col]])
    keep_2023 <- grepl(
      "31\\.12\\.2023|2023-12-31|2023",
      date_values
    )

    if (any(keep_2023, na.rm = TRUE)) {
      population_raw <- population_raw[keep_2023, , drop = FALSE]
      population_name_key <- kr_population_clean_name(names(population_raw))
    }
  }

  # Bei Exporten nach Geschlecht wird nur Insgesamt verwendet.
  sex_col <- grep(
    "geschlecht|sex",
    population_name_key
  )[1L]

  if (!is.na(sex_col)) {
    sex_values <- tolower(as.character(population_raw[[sex_col]]))
    keep_total <- grepl("insgesamt|gesamt|total", sex_values)

    if (any(keep_total, na.rm = TRUE)) {
      population_raw <- population_raw[keep_total, , drop = FALSE]
      population_name_key <- kr_population_clean_name(names(population_raw))
    }
  }

  gkz_col <- grep(
    "gs.*schl|gemeinde.*schl|region.*schl|ags|gkz",
    population_name_key
  )[1L]

  population_col <- grep(
    paste0(
      "bevoelker.*insgesamt|einwohner.*insgesamt|",
      "population.*total|bevoelkerungsstand|bevstd"
    ),
    population_name_key
  )[1L]

  if (is.na(population_col)) {
    population_col <- grep(
      "bevoelker|einwohner|population",
      population_name_key
    )[1L]
  }

  if (is.na(gkz_col) || is.na(population_col)) {
    stop(
      paste0(
        "Bevoelkerungsdatei konnte nicht eindeutig gelesen werden. ",
        "Erwartet werden eine Gemeinde-/AGS-/GKZ-Spalte und eine ",
        "Bevoelkerungs-/Einwohner-Spalte. Datei: ",
        population_path
      ),
      call. = FALSE
    )
  }

  gkz_source <- stringr::str_replace_all(
    as.character(population_raw[[gkz_col]]),
    "[^0-9]",
    ""
  )

  gkz_source <- dplyr::case_when(
    nchar(gkz_source) == 6L ~ paste0("08", gkz_source),
    nchar(gkz_source) == 8L ~ gkz_source,
    TRUE ~ NA_character_
  )

  population_source <- population_raw[[population_col]]
  population_values <- if (is.numeric(population_source)) {
    as.numeric(population_source)
  } else {
    readr::parse_number(
      gsub("\\s", "", as.character(population_source)),
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    )
  }

  population_tab <- tibble::tibble(
    gkz_join = kr_normalize_gkz(gkz_source),
    population = population_values
  ) |>
    dplyr::filter(
      !is.na(gkz_join),
      !is.na(population),
      startsWith(gkz_join, map_prefix)
    ) |>
    dplyr::distinct(gkz_join, population) |>
    dplyr::mutate(
      map_key = substr(
        gkz_join,
        start = 1L,
        stop = map_gkz_digits
      )
    ) |>
    dplyr::group_by(map_key) |>
    dplyr::summarise(
      population = sum(population, na.rm = TRUE),
      n_municipalities = dplyr::n(),
      .groups = "drop"
    )

  ctx <- tc_write_table(
    tab = population_tab,
    table_id = "04_05_population_by_gkz",
    ctx = ctx,
    output_category = "aggregate_table",
    threshold = FALSE
  )
