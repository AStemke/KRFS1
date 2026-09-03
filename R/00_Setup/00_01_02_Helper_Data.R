# 00_01_02_Helper_Data.R -----------------------------
## Zweck --------------------------
# Robuste Hilfsfunktionen fuer Datumsvariablen, IDs, ICD-10, UICC, Altersgruppen und Analyse-Kohorten.
# Dummydaten enthalten teils Monats-/Jahresangaben; diese werden auf Monatsmitte gesetzt.

# Allgemeine Datenhelper -----------------------------
## Erster nicht fehlender Wert --------------------------
first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA)
  x[[1]]
}

## Character-Konvertierung --------------------------
kr_as_character <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# Datumshelper -----------------------------
## Datum robust parsen --------------------------
kr_parse_date <- function(x, impute_day = 15L) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))

  out <- rep(as.Date(NA), length(x))

  if (is.numeric(x) || is.integer(x)) {
    idx <- !is.na(x)
    out[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(out)
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NULL")] <- NA_character_

  idx_full <- !is.na(x_chr) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x_chr)
  out[idx_full] <- as.Date(x_chr[idx_full])

  idx_dmy <- !is.na(x_chr) & grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$", x_chr)
  if (any(idx_dmy)) {
    out[idx_dmy] <- as.Date(x_chr[idx_dmy], format = "%d.%m.%Y")
  }

  idx_my <- !is.na(x_chr) & grepl("^[0-9]{1,2}\\.[0-9]{4}$", x_chr)
  if (any(idx_my)) {
    parts <- strsplit(x_chr[idx_my], "\\.")
    month <- as.integer(vapply(parts, `[[`, character(1), 1))
    year <- as.integer(vapply(parts, `[[`, character(1), 2))
    ok <- !is.na(month) & !is.na(year) & month >= 1 & month <= 12
    tmp <- rep(as.Date(NA), length(month))
    tmp[ok] <- as.Date(sprintf("%04d-%02d-%02d", year[ok], month[ok], impute_day))
    out[idx_my] <- tmp
  }

  idx_ym <- !is.na(x_chr) & grepl("^[0-9]{4}-[0-9]{1,2}$", x_chr)
  if (any(idx_ym)) {
    parts <- strsplit(x_chr[idx_ym], "-")
    year <- as.integer(vapply(parts, `[[`, character(1), 1))
    month <- as.integer(vapply(parts, `[[`, character(1), 2))
    ok <- !is.na(month) & !is.na(year) & month >= 1 & month <= 12
    tmp <- rep(as.Date(NA), length(month))
    tmp[ok] <- as.Date(sprintf("%04d-%02d-%02d", year[ok], month[ok], impute_day))
    out[idx_ym] <- tmp
  }

  out
}

## Jahr aus Datum --------------------------
kr_year <- function(x) {
  d <- kr_parse_date(x)
  as.integer(format(d, "%Y"))
}

# ICD-10 und Kategorien -----------------------------
## Dreistelligen ICD-10-Code bilden --------------------------
kr_icd3 <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[!is.na(x) & x == ""] <- NA_character_
  substr(x, 1, 3)
}

## UICC-Stadium vereinheitlichen --------------------------
kr_normalize_uicc <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "NULL")] <- NA_character_

  x <- gsub("^UICC[[:space:]]*", "", x)
  x <- gsub("^STADIUM[[:space:]]*", "", x)
  x <- gsub("[[:space:]._-]", "", x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("^0", x) ~ "0",
    grepl("^IV", x) ~ "IV",
    grepl("^III", x) ~ "III",
    grepl("^II", x) ~ "II",
    grepl("^I", x) ~ "I",
    x %in% c("X", "UNBEKANNT", "UNK", "UNKNOWN") ~ "X",
    TRUE ~ x
  )
}

## Geschlecht normalisieren --------------------------
kr_normalize_sex <- function(x) {
  x <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("M", "MAENNLICH", "MANN", "1") ~ "maennlich",
    x %in% c("W", "F", "WEIBLICH", "FRAU", "2") ~ "weiblich",
    x %in% c("D", "DIVERS", "3") ~ "divers",
    is.na(x) | x == "" ~ NA_character_,
    TRUE ~ "unbekannt"
  )
}

## Altersgruppe bilden --------------------------
kr_make_age_group <- function(age) {
  cut(
    as.numeric(age),
    breaks = config$explore$age_breaks,
    labels = config$explore$age_labels,
    right = FALSE
  )
}

# Geografische Variablen -----------------------------
## GKZ normalisieren --------------------------
kr_normalize_gkz <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x <- ifelse(!is.na(x) & nchar(x) == 7, paste0("0", x), x)
  x <- ifelse(!is.na(x) & nchar(x) < 8, stringr::str_pad(x, width = 8, side = "left", pad = "0"), x)
  x
}

## PLZ normalisieren --------------------------
kr_normalize_plz <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  ifelse(!is.na(x), stringr::str_pad(x, width = 5, side = "left", pad = "0"), NA_character_)
}

## Regionale Ausgabe filtern --------------------------
# Diese Funktion ist nur fuer explizit regionale Tabellen oder Karten gedacht.
# Die allgemeine Analysepopulation wird dadurch nicht eingeschraenkt.
kr_filter_regional_output <- function(tumor_base) {
  stopifnot(is.data.frame(tumor_base))

  region_config <- config$analysis$regional_output

  if (is.null(region_config) || !isTRUE(region_config$enabled)) {
    return(tumor_base)
  }

  if (!"gkz_diagnose" %in% names(tumor_base)) {
    stop(
      "Fuer die regionale Ausgabe fehlt `gkz_diagnose` in der Tumorbasis.",
      call. = FALSE
    )
  }

  gkz_prefix <- as.character(region_config$gkz_prefix)[1L]
  if (is.na(gkz_prefix) || !nzchar(trimws(gkz_prefix))) {
    stop(
      "config$analysis$regional_output$gkz_prefix darf bei aktivierter Regionalausgabe nicht leer sein.",
      call. = FALSE
    )
  }

  gkz <- kr_normalize_gkz(tumor_base$gkz_diagnose)
  keep <- !is.na(gkz) & startsWith(gkz, gkz_prefix)

  tumor_base[keep, , drop = FALSE]
}

## Regionale Ausgabe beschriften --------------------------
kr_regional_output_label <- function() {
  region_config <- config$analysis$regional_output

  if (is.null(region_config) || !isTRUE(region_config$enabled)) {
    return("")
  }

  region_label <- as.character(region_config$label)[1L]
  if (is.na(region_label) || !nzchar(trimws(region_label))) {
    return(paste0("GKZ ", region_config$gkz_prefix))
  }

  region_label
}

# Mathematische Helper -----------------------------
## Sichere Division --------------------------
safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

# Analyse-Kohorten -----------------------------
## Tumorbasis nach ICD-10 filtern --------------------------
kr_filter_tumor_cohort <- function(tumor_base, icd10_3_keep = character(0)) {
  stopifnot(is.data.frame(tumor_base), "icd10_3" %in% names(tumor_base))

  # Die Distanzdefinition wird bei jedem Modul erneut auf die geladene Basis
  # angewendet. So greifen geaenderte Default- oder ICD-spezifische Grenzen auch
  # bei separat gestarteten Analysemodulen und aelteren Prepare-RDS-Dateien.
  if (exists("kr_apply_near_care_config", mode = "function")) {
    tumor_base <- kr_apply_near_care_config(tumor_base)
  }

  # Die Diagnose-GKZ wird hier bewusst NICHT regional gefiltert. Auch Faelle
  # ausserhalb der Posterregion bleiben in Descriptive, Access und Stats erhalten;
  # ihre Distanz ist Teil der Versorgungsanalyse.
  icd10_3_keep <- unique(toupper(trimws(as.character(icd10_3_keep))))
  icd10_3_keep <- icd10_3_keep[!is.na(icd10_3_keep) & nzchar(icd10_3_keep)]

  if (length(icd10_3_keep) == 0L) {
    return(tumor_base)
  }

  tumor_base |>
    dplyr::filter(icd10_3 %in% icd10_3_keep)
}

## Patientenbasis aus gefilterter Tumorbasis neu bilden --------------------------
kr_build_patient_base <- function(tumor_base) {
  stopifnot(is.data.frame(tumor_base), "LNGREGISTERNR" %in% names(tumor_base))

  tumor_base |>
    dplyr::group_by(LNGREGISTERNR) |>
    dplyr::summarise(
      n_tumors = dplyr::n_distinct(LNGTUMORID),
      first_diagnosis_date = suppressWarnings(min(diagnosis_date, na.rm = TRUE)),
      first_diagnosis_year = suppressWarnings(min(diagnosis_year, na.rm = TRUE)),
      sex = first_non_missing(sex),
      age_at_first_diag = first_non_missing(age_at_diag),
      first_gkz_diagnose = first_non_missing(gkz_diagnose),
      min_distance_km = suppressWarnings(min(distance_km, na.rm = TRUE)),
      any_near_care = any(near_care, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      first_diagnosis_date = dplyr::if_else(is.infinite(first_diagnosis_date), as.Date(NA), first_diagnosis_date),
      first_diagnosis_year = dplyr::if_else(is.infinite(first_diagnosis_year), NA_integer_, as.integer(first_diagnosis_year)),
      min_distance_km = dplyr::if_else(is.infinite(min_distance_km), NA_real_, min_distance_km)
    )
}
