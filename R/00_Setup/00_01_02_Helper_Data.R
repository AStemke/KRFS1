# 00_01_02_Helper_Data.R -----------
# Zweck:
# - Robuste Hilfsfunktionen fuer Datumsvariablen, IDs, ICD-10, Altersgruppen
# - Dummydaten enthalten teils Monats-/Jahresangaben; diese werden auf Monatsmitte gesetzt

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA)
  x[[1]]
}

kr_as_character <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

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

kr_year <- function(x) {
  d <- kr_parse_date(x)
  as.integer(format(d, "%Y"))
}

kr_icd3 <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[!is.na(x) & x == ""] <- NA_character_
  substr(x, 1, 3)
}

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

kr_make_age_group <- function(age) {
  cut(
    as.numeric(age),
    breaks = config$explore$age_breaks,
    labels = config$explore$age_labels,
    right = FALSE
  )
}

kr_normalize_gkz <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x <- ifelse(!is.na(x) & nchar(x) == 7, paste0("0", x), x)
  x <- ifelse(!is.na(x) & nchar(x) < 8, stringr::str_pad(x, width = 8, side = "left", pad = "0"), x)
  x
}

kr_normalize_plz <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  ifelse(!is.na(x), stringr::str_pad(x, width = 5, side = "left", pad = "0"), NA_character_)
}

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}
