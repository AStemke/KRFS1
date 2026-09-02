# Ergebnisuebersicht -----------------------------
## Zweck --------------------------
# Kompakte Ergebnisblaetter fuer die ausgewaehlten Fokus-Entitaeten.
# Die Tabellen werden einmal im Lauf von Selektiv_Gesamt erzeugt und enthalten
# Gesamtwerte sowie Stratifizierungen nach Diagnosejahr und Wohnregion.

# Hilfsfunktionen -----------------------------
## Wohnregion aus Diagnose-GKZ ableiten --------------------------
kr_result_overview_region <- function(gkz_diagnose) {
  region_config <- config$analysis$result_overview_region
  gkz <- kr_normalize_gkz(gkz_diagnose)

  dplyr::case_when(
    is.na(gkz) | gkz == "00000000" ~ region_config$label_missing,
    startsWith(gkz, region_config$stuttgart_gkz_prefix) ~ region_config$label_stuttgart,
    startsWith(gkz, region_config$bw_gkz_prefix) ~ region_config$label_bw_other,
    TRUE ~ region_config$label_outside_bw
  )
}

## Kennzahlen fuer beliebige Gruppierung --------------------------
kr_result_overview_summary <- function(data, group_vars) {
  out <- data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      n_tumors = dplyr::n_distinct(LNGTUMORID, na.rm = TRUE),
      n_patients = dplyr::n_distinct(LNGREGISTERNR, na.rm = TRUE),
      n_with_distance = sum(!is.na(distance_km)),
      age_median = if (all(is.na(age_at_diag))) {
        NA_real_
      } else {
        stats::median(age_at_diag, na.rm = TRUE)
      },
      distance_median_km = if (all(is.na(distance_km))) {
        NA_real_
      } else {
        stats::median(distance_km, na.rm = TRUE)
      },
      near_care_limit_km = dplyr::first(
        near_care_limit_km[!is.na(near_care_limit_km)],
        default = NA_real_
      ),
      n_near = sum(near_care %in% TRUE, na.rm = TRUE),
      n_not_near = sum(
        !is.na(distance_km) & near_care %in% FALSE,
        na.rm = TRUE
      ),
      pct_near = 100 * safe_divide(n_near, n_with_distance),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      icd10_3 = factor(
        icd10_3,
        levels = config$analysis$result_overview_icd10_3
      )
    )

  sort_vars <- intersect(
    c("icd10_3", "diagnosis_year", "region"),
    names(out)
  )

  out <- dplyr::arrange(
    out,
    !!!rlang::syms(sort_vars)
  )

  out |>
    dplyr::mutate(
      icd10_3 = as.character(icd10_3)
    )
}

## Ergebnisblatt schreiben --------------------------
kr_write_result_overview <- function(tab, table_id, label_cols, ctx) {
  tc_write_table(
    tab = tab,
    table_id = table_id,
    ctx = ctx,
    output_category = "descriptive_numeric",
    threshold = TRUE,
    threshold_n_col = c(
      "n_tumors",
      "n_patients",
      "n_with_distance",
      "n_near",
      "n_not_near"
    ),
    threshold_label_cols = label_cols
  )
}

# Ergebnisblaetter -----------------------------
## Fokus-Entitaeten --------------------------
if (identical(config$analysis$active_output_group, "Selektiv_Gesamt")) {
  result_overview_base <- kr_apply_near_care_config(tumor_base_all) |>
    dplyr::filter(
      icd10_3 %in% config$analysis$result_overview_icd10_3
    ) |>
    dplyr::mutate(
      region = kr_result_overview_region(gkz_diagnose)
    )

  # Gesamt je Entitaet.
  result_overview_icd <- kr_result_overview_summary(
    data = result_overview_base,
    group_vars = "icd10_3"
  )

  ctx <- kr_write_result_overview(
    tab = result_overview_icd,
    table_id = "02_14_overview_icd",
    label_cols = "icd10_3",
    ctx = ctx
  )

  # Je Entitaet und Diagnosejahr.
  result_overview_year <- kr_result_overview_summary(
    data = result_overview_base,
    group_vars = c("icd10_3", "diagnosis_year")
  )

  ctx <- kr_write_result_overview(
    tab = result_overview_year,
    table_id = "02_15_overview_year",
    label_cols = c("icd10_3", "diagnosis_year"),
    ctx = ctx
  )

  # Je Entitaet und Wohnregion.
  result_overview_region <- kr_result_overview_summary(
    data = result_overview_base,
    group_vars = c("icd10_3", "region")
  )

  ctx <- kr_write_result_overview(
    tab = result_overview_region,
    table_id = "02_16_overview_region",
    label_cols = c("icd10_3", "region"),
    ctx = ctx
  )

  # Je Entitaet, Diagnosejahr und Wohnregion.
  result_overview_year_region <- kr_result_overview_summary(
    data = result_overview_base,
    group_vars = c("icd10_3", "diagnosis_year", "region")
  )

  ctx <- kr_write_result_overview(
    tab = result_overview_year_region,
    table_id = "02_17_overview_year_region",
    label_cols = c("icd10_3", "diagnosis_year", "region"),
    ctx = ctx
  )
}
