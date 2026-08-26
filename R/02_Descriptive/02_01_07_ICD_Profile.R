# ICD-10-Gruppenprofil -----------------------------
## Standardkennzahlen je dreistelliger ICD-10-Gruppe --------------------------
icd_profile_tab <- kr_describe_by_icd(tumor_base)

ctx <- tc_write_table(
  tab = icd_profile_tab,
  table_id = "02_10_icd_profile",
  ctx = ctx,
  output_category = "descriptive_numeric",
  threshold_n_col = c("n_tumors", "n_patients", "n_near"),
  threshold_label_cols = "icd10_3"
)

# Geschlecht je ICD-10 -----------------------------
## Verteilung --------------------------
icd_sex_tab <- tumor_base |>
  dplyr::mutate(
    icd10_3 = dplyr::if_else(is.na(icd10_3) | icd10_3 == "", "Fehlend", icd10_3),
    sex = dplyr::if_else(is.na(sex) | sex == "", "unbekannt", sex)
  ) |>
  dplyr::group_by(icd10_3, sex) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID, na.rm = TRUE),
    n_patients = dplyr::n_distinct(LNGREGISTERNR, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(icd10_3) |>
  dplyr::mutate(
    pct_tumors = 100 * safe_divide(n_tumors, sum(n_tumors, na.rm = TRUE))
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(icd10_3, sex)

ctx <- tc_write_table(
  tab = icd_sex_tab,
  table_id = "02_11_icd_sex",
  ctx = ctx,
  output_category = "crosstab",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("icd10_3", "sex")
)

# Altersgruppen je ICD-10 -----------------------------
## Verteilung --------------------------
icd_age_tab <- tumor_base |>
  dplyr::mutate(
    icd10_3 = dplyr::if_else(is.na(icd10_3) | icd10_3 == "", "Fehlend", icd10_3),
    age_group = dplyr::if_else(
      is.na(age_group),
      "Fehlend",
      as.character(age_group)
    )
  ) |>
  dplyr::group_by(icd10_3, age_group) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID, na.rm = TRUE),
    n_patients = dplyr::n_distinct(LNGREGISTERNR, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(icd10_3) |>
  dplyr::mutate(
    pct_tumors = 100 * safe_divide(n_tumors, sum(n_tumors, na.rm = TRUE))
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(icd10_3, age_group)

ctx <- tc_write_table(
  tab = icd_age_tab,
  table_id = "02_12_icd_age_group",
  ctx = ctx,
  output_category = "crosstab",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("icd10_3", "age_group")
)

# UICC je ICD-10 -----------------------------
## Verteilung --------------------------
icd_uicc_tab <- tumor_base |>
  dplyr::mutate(
    icd10_3 = dplyr::if_else(is.na(icd10_3) | icd10_3 == "", "Fehlend", icd10_3),
    uicc = dplyr::if_else(is.na(uicc) | trimws(uicc) == "", "Fehlend", uicc)
  ) |>
  dplyr::group_by(icd10_3, uicc) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID, na.rm = TRUE),
    n_patients = dplyr::n_distinct(LNGREGISTERNR, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(icd10_3, uicc)

ctx <- tc_write_table(
  tab = icd_uicc_tab,
  table_id = "02_13_icd_uicc",
  ctx = ctx,
  output_category = "crosstab",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("icd10_3", "uicc")
)
