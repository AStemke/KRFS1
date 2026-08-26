# 02_00-Descriptive_Setup.R -----------------------------
## Zweck --------------------------
# Hilfsfunktionen fuer standardisierte deskriptive Auswertungen.
# Die Funktionen unterscheiden numerische, kategoriale, Datums- und ID-Variablen.

# Variablentypen -----------------------------
## Variablentyp bestimmen --------------------------
kr_descriptive_type <- function(x, variable_name = "") {
  variable_name <- toupper(variable_name)

  if (grepl("(^|_)(ID|NR)$|^LNG.*ID$|^LNGREGISTERNR$", variable_name)) {
    return("id")
  }

  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("date")
  }

  if (is.logical(x)) {
    return("categorical")
  }

  if (is.numeric(x) || is.integer(x)) {
    return("numeric")
  }

  "categorical"
}

# Allgemeine Helper -----------------------------
## Fehlende Werte erkennen --------------------------
kr_is_missing <- function(x) {
  if (is.character(x) || is.factor(x)) {
    return(is.na(x) | trimws(as.character(x)) == "")
  }

  is.na(x)
}

## Sichere Quantile --------------------------
kr_safe_quantile <- function(x, prob) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

# Variablenprofil -----------------------------
## Eine Variable zusammenfassen --------------------------
kr_describe_variable <- function(x, variable_name, data_level) {
  type <- kr_descriptive_type(x, variable_name)
  missing <- kr_is_missing(x)
  x_non_missing <- x[!missing]

  out <- data.frame(
    data_level = data_level,
    variable = variable_name,
    variable_type = type,
    class = paste(class(x), collapse = "/"),
    n_total = length(x),
    n_non_missing = sum(!missing),
    n_missing = sum(missing),
    pct_missing = if (length(x) > 0L) 100 * mean(missing) else NA_real_,
    n_unique = length(unique(x_non_missing)),
    stringsAsFactors = FALSE
  )

  if (type == "numeric") {
    x_num <- suppressWarnings(as.numeric(x_non_missing))
    x_num <- x_num[is.finite(x_num)]

    out$mean <- if (length(x_num) > 0L) mean(x_num) else NA_real_
    out$sd <- if (length(x_num) > 1L) stats::sd(x_num) else NA_real_
    out$min <- if (length(x_num) > 0L) min(x_num) else NA_real_
    out$q1 <- kr_safe_quantile(x_num, 0.25)
    out$median <- kr_safe_quantile(x_num, 0.50)
    out$q3 <- kr_safe_quantile(x_num, 0.75)
    out$max <- if (length(x_num) > 0L) max(x_num) else NA_real_
  } else {
    out$mean <- NA_real_
    out$sd <- NA_real_
    out$min <- NA_real_
    out$q1 <- NA_real_
    out$median <- NA_real_
    out$q3 <- NA_real_
    out$max <- NA_real_
  }

  if (type == "date") {
    x_date <- as.Date(x_non_missing)
    out$first_value <- if (length(x_date) > 0L) as.character(min(x_date)) else NA_character_
    out$last_value <- if (length(x_date) > 0L) as.character(max(x_date)) else NA_character_
  } else {
    out$first_value <- NA_character_
    out$last_value <- NA_character_
  }

  out
}

## Datensatz variablenweise zusammenfassen --------------------------
kr_describe_data <- function(data, data_level) {
  stopifnot(is.data.frame(data))

  dplyr::bind_rows(
    lapply(
      names(data),
      function(variable_name) {
        kr_describe_variable(
          x = data[[variable_name]],
          variable_name = variable_name,
          data_level = data_level
        )
      }
    )
  )
}

# Kategoriale Variablen -----------------------------
## Haeufigkeiten fuer geeignete Variablen --------------------------
kr_describe_categories <- function(data, data_level, max_levels = 25L) {
  stopifnot(is.data.frame(data))

  tabs <- lapply(
    names(data),
    function(variable_name) {
      x <- data[[variable_name]]
      type <- kr_descriptive_type(x, variable_name)

      if (type != "categorical") {
        return(NULL)
      }

      x_chr <- as.character(x)
      x_chr[is.na(x_chr) | trimws(x_chr) == ""] <- "Fehlend"

      if (dplyr::n_distinct(x_chr) > max_levels) {
        return(NULL)
      }

      data.frame(value = x_chr, stringsAsFactors = FALSE) |>
        dplyr::count(value, name = "n") |>
        dplyr::mutate(
          data_level = data_level,
          variable = variable_name,
          pct = 100 * n / sum(n),
          .before = 1
        )
    }
  )

  out <- dplyr::bind_rows(tabs)

  if (nrow(out) == 0L) {
    out <- data.frame(
      data_level = character(0),
      variable = character(0),
      value = character(0),
      n = integer(0),
      pct = numeric(0)
    )
  }

  out
}

# ICD-Gruppen -----------------------------
## Standardbeschreibung je ICD-10-Gruppe --------------------------
kr_describe_by_icd <- function(tumor_base) {
  stopifnot(is.data.frame(tumor_base), "icd10_3" %in% names(tumor_base))

  tumor_base |>
    dplyr::mutate(
      icd10_3 = dplyr::if_else(is.na(icd10_3) | icd10_3 == "", "Fehlend", icd10_3)
    ) |>
    dplyr::group_by(icd10_3) |>
    dplyr::summarise(
      n_tumors = dplyr::n_distinct(LNGTUMORID, na.rm = TRUE),
      n_patients = dplyr::n_distinct(LNGREGISTERNR, na.rm = TRUE),
      age_mean = if (all(is.na(age_at_diag))) NA_real_ else mean(age_at_diag, na.rm = TRUE),
      age_sd = if (sum(!is.na(age_at_diag)) > 1L) stats::sd(age_at_diag, na.rm = TRUE) else NA_real_,
      age_median = if (all(is.na(age_at_diag))) NA_real_ else stats::median(age_at_diag, na.rm = TRUE),
      age_q1 = kr_safe_quantile(age_at_diag, 0.25),
      age_q3 = kr_safe_quantile(age_at_diag, 0.75),
      distance_mean_km = if (all(is.na(distance_km))) NA_real_ else mean(distance_km, na.rm = TRUE),
      distance_median_km = if (all(is.na(distance_km))) NA_real_ else stats::median(distance_km, na.rm = TRUE),
      n_near = sum(near_care %in% TRUE, na.rm = TRUE),
      pct_near = 100 * safe_divide(n_near, n_tumors),
      .groups = "drop"
    ) |>
    dplyr::arrange(icd10_3)
}
