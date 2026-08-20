# 02_01_04_ICD_UICC.R -----------
# Zweck: ICD-10 und UICC-Verteilung

icd_tab <- tumor_base |>
  dplyr::filter(!is.na(icd10_3)) |>
  dplyr::group_by(icd10_3) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n_tumors)) |>
  dplyr::slice_head(n = 20)

ctx <- tc_write_table(
  tab = icd_tab,
  table_id = "02_04_icd10_top20",
  ctx = ctx,
  output_category = "aggregate_table",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("icd10_3")
)

uicc_tab <- tumor_base |>
  dplyr::mutate(uicc = dplyr::if_else(is.na(uicc) | uicc == "", "unbekannt", uicc)) |>
  dplyr::group_by(uicc) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    .groups = "drop"
  ) |>
  dplyr::arrange(uicc)

ctx <- tc_write_table(
  tab = uicc_tab,
  table_id = "02_04_uicc",
  ctx = ctx,
  output_category = "aggregate_table",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("uicc")
)
