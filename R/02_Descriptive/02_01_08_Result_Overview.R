# Ergebnisuebersicht -----------------------------
## Kompakte Kennzahlen der ausgewaehlten Entitaeten --------------------------
# Die Uebersicht wird einmal im Lauf von Selektiv_Gesamt erzeugt, greift aber
# bewusst auf tumor_base_all zurueck. So koennen auch Fokus-Entitaeten enthalten
# sein, die nicht Teil der selektiven Projektpopulation sind.

if (identical(config$analysis$active_output_group, "Selektiv_Gesamt")) {
  result_overview_base <- tumor_base_all |>
    dplyr::filter(
      icd10_3 %in% config$analysis$result_overview_icd10_3
    )

  result_overview_tab <- result_overview_base |>
    dplyr::group_by(icd10_3) |>
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
      pct_near = 100 * safe_divide(n_near, n_with_distance),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      icd10_3 = factor(
        icd10_3,
        levels = config$analysis$result_overview_icd10_3
      )
    ) |>
    dplyr::arrange(icd10_3) |>
    dplyr::mutate(
      icd10_3 = as.character(icd10_3)
    )

  ctx <- tc_write_table(
    tab = result_overview_tab,
    table_id = "02_14_result_overview_icd",
    ctx = ctx,
    output_category = "descriptive_numeric",
    threshold = TRUE,
    threshold_n_col = c(
      "n_tumors",
      "n_patients",
      "n_with_distance",
      "n_near"
    ),
    threshold_label_cols = "icd10_3"
  )
}
