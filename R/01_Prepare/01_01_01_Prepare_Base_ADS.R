# Datenaufbereitung -----------------------------
## Zweck --------------------------
# Standardisierte Variablen fuer Meldungs-, Tumor- und Patientenebene erzeugen.

# Meldungsebene -----------------------------
## Rohdaten standardisieren --------------------------
dat_msg <- dat_raw
names(dat_msg) <- trimws(names(dat_msg))

id_vars <- intersect(c("LNGTUMORID", "LNGREGISTERNR", "LNGMELDUNGID"), names(dat_msg))
for (v in id_vars) dat_msg[[v]] <- kr_as_character(dat_msg[[v]])

if ("GKZ_DIAGNOSE" %in% names(dat_msg)) dat_msg$GKZ_DIAGNOSE <- kr_normalize_gkz(dat_msg$GKZ_DIAGNOSE)
if ("GKZ_AKTUELL" %in% names(dat_msg)) dat_msg$GKZ_AKTUELL <- kr_normalize_gkz(dat_msg$GKZ_AKTUELL)
if ("PLZ_DIAGNOSE" %in% names(dat_msg)) dat_msg$PLZ_DIAGNOSE <- kr_normalize_plz(dat_msg$PLZ_DIAGNOSE)
if ("PLZ_PATIENT" %in% names(dat_msg)) dat_msg$PLZ_PATIENT <- kr_normalize_plz(dat_msg$PLZ_PATIENT)
if ("PLZ_Krankenhaus" %in% names(dat_msg)) dat_msg$PLZ_Krankenhaus <- kr_normalize_plz(dat_msg$PLZ_Krankenhaus)

## Standardvariablen --------------------------
dat_msg <- dat_msg |>
  dplyr::mutate(
    diagnosis_date = if ("DIAGNOSEDATUM" %in% names(dat_msg)) kr_parse_date(DIAGNOSEDATUM) else as.Date(NA),
    diagnosis_year = as.integer(format(diagnosis_date, "%Y")),
    sex = if ("STRGESCHLECHT" %in% names(dat_msg)) kr_normalize_sex(STRGESCHLECHT) else NA_character_,
    age_at_diag = if ("Alter" %in% names(dat_msg)) suppressWarnings(as.numeric(Alter)) else NA_real_,
    age_group = kr_make_age_group(age_at_diag),
    icd10 = if ("STRDIAGNOSEICD10" %in% names(dat_msg)) toupper(trimws(as.character(STRDIAGNOSEICD10))) else NA_character_,
    icd10_3 = kr_icd3(icd10),
    uicc = if ("STRTNMSTADIUM_UICC" %in% names(dat_msg)) kr_normalize_uicc(STRTNMSTADIUM_UICC) else NA_character_,
    therapy_type = if ("STRTHERAPIEART" %in% names(dat_msg)) as.character(STRTHERAPIEART) else NA_character_,
    therapy_start_date = if ("THERAPIESTART" %in% names(dat_msg)) kr_parse_date(THERAPIESTART) else as.Date(NA),
    death_date = if ("DTMSTERBEMONAT" %in% names(dat_msg)) kr_parse_date(DTMSTERBEMONAT) else as.Date(NA),
    report_date = if ("DTMMELDUNG" %in% names(dat_msg)) kr_parse_date(DTMMELDUNG) else as.Date(NA),
    distance_km = if ("Distanz_km" %in% names(dat_msg)) suppressWarnings(as.numeric(Distanz_km)) else NA_real_,
    near_care_limit_km = kr_near_care_limit_km(icd10_3),
    near_care = !is.na(distance_km) & distance_km <= near_care_limit_km,
    distance_group = cut(
      distance_km,
      breaks = config$analysis$distance_breaks_km,
      labels = config$analysis$distance_labels,
      right = FALSE
    )
  )

# Filter -----------------------------
## Optionale Analysefilter --------------------------
if (!is.null(config$analysis$diagnosis_year_min)) {
  dat_msg <- dat_msg |> dplyr::filter(is.na(diagnosis_year) | diagnosis_year >= config$analysis$diagnosis_year_min)
}
if (!is.null(config$analysis$diagnosis_year_max)) {
  dat_msg <- dat_msg |> dplyr::filter(is.na(diagnosis_year) | diagnosis_year <= config$analysis$diagnosis_year_max)
}
if (!is.null(config$analysis$icd10_prefix_keep)) {
  dat_msg <- dat_msg |>
    dplyr::filter(stringr::str_starts(icd10, paste0("^(", paste(config$analysis$icd10_prefix_keep, collapse = "|"), ")")))
}

# Tumorebene -----------------------------
## Meldungen zu Tumoren aggregieren --------------------------
tumor_base <- dat_msg |>
  dplyr::group_by(LNGTUMORID) |>
  dplyr::summarise(
    LNGREGISTERNR = first_non_missing(LNGREGISTERNR),
    diagnosis_date = suppressWarnings(min(diagnosis_date, na.rm = TRUE)),
    diagnosis_year = suppressWarnings(min(diagnosis_year, na.rm = TRUE)),
    sex = first_non_missing(sex),
    age_at_diag = first_non_missing(age_at_diag),
    age_group = first_non_missing(as.character(age_group)),
    icd10 = first_non_missing(icd10),
    icd10_3 = first_non_missing(icd10_3),
    uicc = first_non_missing(uicc),
    gkz_diagnose = first_non_missing(GKZ_DIAGNOSE),
    gkz_current = first_non_missing(GKZ_AKTUELL),
    plz_diagnose = first_non_missing(PLZ_DIAGNOSE),
    plz_patient = first_non_missing(PLZ_PATIENT),
    distance_km = suppressWarnings(min(distance_km, na.rm = TRUE)),
    near_care_limit_km = first_non_missing(near_care_limit_km),
    near_care = any(near_care, na.rm = TRUE),
    distance_group = first_non_missing(as.character(distance_group)),
    n_messages = dplyr::n(),
    n_meldung = dplyr::n_distinct(LNGMELDUNGID, na.rm = TRUE),
    has_op = any(therapy_type == "OP", na.rm = TRUE),
    has_st = any(therapy_type == "ST", na.rm = TRUE),
    has_sy = any(therapy_type == "SY", na.rm = TRUE),
    has_metastasis = any(!is.na(STRMETASTASE) & STRMETASTASE != "", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    diagnosis_date = dplyr::if_else(is.infinite(diagnosis_date), as.Date(NA), diagnosis_date),
    diagnosis_year = dplyr::if_else(is.infinite(diagnosis_year), NA_integer_, as.integer(diagnosis_year)),
    distance_km = dplyr::if_else(is.infinite(distance_km), NA_real_, distance_km),
    age_group = factor(age_group, levels = config$explore$age_labels),
    distance_group = factor(distance_group, levels = config$analysis$distance_labels)
  )

# Patientenebene -----------------------------
## Tumoren zu Patienten aggregieren --------------------------
patient_base <- tumor_base |>
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

# Speichern -----------------------------
## Analysebasen --------------------------
saveRDS(dat_msg, file = file.path(ctx$paths$rds, "dat_msg.rds"))
saveRDS(tumor_base, file = file.path(ctx$paths$rds, "tumor_base.rds"))
saveRDS(patient_base, file = file.path(ctx$paths$rds, "patient_base.rds"))

prep_overview <- data.frame(
  object = c("dat_msg", "tumor_base", "patient_base"),
  n_rows = c(nrow(dat_msg), nrow(tumor_base), nrow(patient_base)),
  n_cols = c(ncol(dat_msg), ncol(tumor_base), ncol(patient_base))
)

ctx <- tc_write_table(
  tab = prep_overview,
  table_id = "01_01_prepare_overview",
  ctx = ctx,
  output_category = "technical_overview",
  threshold = FALSE
)
