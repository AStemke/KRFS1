# 01_01_07_ICD10_Top10
# Zweck: Häufigste ICD-10 Diagnosen

# Tabelle: ICD-10 Top 10---------------------------
icd10_tab <- dat %>%
  dplyr::filter(!is.na(STRDIAGNOSEICD10)) %>%
  dplyr::count(STRDIAGNOSEICD10, name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::slice_head(n = 10)

ctx <<- tc_write_table(
  tab = icd10_tab,
  table_id = "01_07_icd10_top10",
  ctx = ctx,
  threshold = TRUE,
  threshold_label_cols = c("STRDIAGNOSEICD10")
)

## Grafik: ICD-10 Top 10-------------------------------------
p_icd10 <- ggplot2::ggplot(
  icd10_tab,
  ggplot2::aes(x = stats::reorder(as.character(STRDIAGNOSEICD10), n), y = n)
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "ICD-10 Top 10", x = NULL, y = "Anzahl")

ctx <<- tc_write_figure(
  plot = p_icd10,
  fig_id = "01_07_icd10",
  filename = "icd10_top10_bar",
  ctx = ctx,
  threshold = TRUE,
  threshold_tab = icd10_tab,
  threshold_label_cols = c("STRDIAGNOSEICD10")
)
