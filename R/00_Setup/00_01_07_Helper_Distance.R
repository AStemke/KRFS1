# Distanzgrenzen -----------------------------
## Entitaetsspezifischen Grenzwert bestimmen --------------------------
kr_near_care_limit_km <- function(icd10_3) {
  default_km <- as.numeric(config$analysis$near_care_default_km)
  limits <- config$analysis$near_care_km_by_icd

  out <- rep(default_km, length(icd10_3))

  if (length(limits) == 0L) {
    return(out)
  }

  if (is.null(names(limits)) || any(!nzchar(names(limits)))) {
    stop(
      "config$analysis$near_care_km_by_icd muss ein benannter numerischer Vektor sein.",
      call. = FALSE
    )
  }

  limits_num <- as.numeric(limits)
  names(limits_num) <- names(limits)

  matched <- match(
    toupper(as.character(icd10_3)),
    toupper(names(limits_num))
  )
  has_specific_limit <- !is.na(matched)

  out[has_specific_limit] <- limits_num[matched[has_specific_limit]]
  out
}

## Lesbare Distanzangabe fuer Titel --------------------------
kr_near_care_distance_label <- function(
    icd10_3 = config$analysis$active_icd10_3
) {
  default_km <- as.numeric(config$analysis$near_care_default_km)
  limits <- config$analysis$near_care_km_by_icd
  icd10_3 <- unique(toupper(as.character(icd10_3)))
  icd10_3 <- icd10_3[!is.na(icd10_3) & nzchar(icd10_3)]

  format_km <- function(x) {
    format(
      x,
      trim = TRUE,
      scientific = FALSE,
      decimal.mark = ","
    )
  }

  if (length(icd10_3) == 1L) {
    limit_km <- kr_near_care_limit_km(icd10_3)[1L]
    return(paste0("bis ", format_km(limit_km), " km"))
  }

  if (length(limits) == 0L) {
    return(paste0("bis ", format_km(default_km), " km"))
  }

  limits_num <- as.numeric(limits)
  names(limits_num) <- toupper(names(limits))

  if (length(icd10_3) > 0L) {
    limits_num <- limits_num[names(limits_num) %in% icd10_3]
  }

  if (length(limits_num) == 0L) {
    return(paste0("bis ", format_km(default_km), " km"))
  }

  specific_text <- paste0(
    names(limits_num),
    ": ",
    vapply(limits_num, format_km, character(1)),
    " km",
    collapse = "; "
  )

  paste0(
    "Default ",
    format_km(default_km),
    " km; ",
    specific_text
  )
}
