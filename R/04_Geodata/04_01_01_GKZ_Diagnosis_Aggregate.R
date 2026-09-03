# Wohnortaggregation -----------------------------
## Zweck --------------------------
# Aggregiert Tumoren, Patienten und Distanzkennzahlen nach der konfigurierten
# Wohn-GKZ. Standard ist GKZ_AKTUELL, passend zur Distanz Wohnort aktuell -> Krankenhaus.

# Wohn-GKZ -----------------------------
## Kennzahlen --------------------------
residence_gkz_var <- config$analysis$analysis_region$gkz_var

if (!residence_gkz_var %in% names(tumor_base)) {
  stop(
    paste0(
      "Fuer die Wohnortaggregation fehlt die konfigurierte GKZ-Variable `",
      residence_gkz_var,
      "`."
    ),
    call. = FALSE
  )
}

gkz_residence_tab <- tumor_base |>
  dplyr::mutate(
    gkz_residence = kr_normalize_gkz(.data[[residence_gkz_var]])
  ) |>
  dplyr::filter(
    !is.na(gkz_residence)
  ) |>
  dplyr::group_by(
    gkz_residence
  ) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    median_distance_km = if (all(is.na(distance_km))) {
      NA_real_
    } else {
      stats::median(distance_km, na.rm = TRUE)
    },
    pct_near_care = if (all(is.na(near_care))) {
      NA_real_
    } else {
      mean(near_care, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Die Tabelle wird zeilenweise datenschutzgeprueft gespeichert.
ctx <- tc_write_table(
  tab = gkz_residence_tab,
  table_id = "04_01_gkz_residence_aggregate",
  ctx = ctx,
  output_category = "map_area",
  threshold_n_col = c(
    "n_tumors",
    "n_patients"
  ),
  threshold_label_cols = "gkz_residence"
)
