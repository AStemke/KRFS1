# 05_01_02_Test_Distance_By_UICC.R -----------
# Zweck: Distanzvergleich nach UICC Stadium mit Kruskal-Wallis-Test

uicc_dist_summary <- tumor_base |>
  dplyr::filter(!is.na(uicc), !is.na(distance_km)) |>
  dplyr::group_by(uicc) |>
  dplyr::summarise(
    n = dplyr::n_distinct(LNGTUMORID),
    median_km = stats::median(distance_km, na.rm = TRUE),
    q25_km = stats::quantile(distance_km, 0.25, na.rm = TRUE),
    q75_km = stats::quantile(distance_km, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

ctx <- tc_write_table(
  tab = uicc_dist_summary,
  table_id = "05_02_distance_by_uicc_summary",
  ctx = ctx,
  output_category = "descriptive_numeric",
  threshold_n_col = "n",
  threshold_label_cols = c("uicc")
)

if (passes_threshold_category(uicc_dist_summary, "descriptive_numeric", n_col = "n", label_cols = c("uicc"), object_name = "05_02_distance_by_uicc_test", write_log = FALSE)) {
  model_dat <- tumor_base |>
    dplyr::filter(!is.na(uicc), !is.na(distance_km))
  
  if (dplyr::n_distinct(model_dat$uicc) >= 2) {
    kw <- stats::kruskal.test(distance_km ~ as.factor(uicc), data = model_dat)
    kw_tab <- data.frame(
      test = "kruskal_wallis",
      n_total = nrow(model_dat),
      statistic = unname(kw$statistic),
      df = unname(kw$parameter),
      p_value = kw$p.value,
      method = kw$method,
      stringsAsFactors = FALSE
    )
    ctx <- tc_write_table(
      tab = kw_tab,
      table_id = "05_02_distance_by_uicc_test",
      ctx = ctx,
      output_category = "statistical_test",
      threshold_n_col = "n_total",
      threshold_label_cols = c("test")
    )
  }
}
