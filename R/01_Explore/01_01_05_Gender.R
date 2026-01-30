# 01_01_05_Gender
# Zweck: Geschlechterverteilung

# Tabelle: Geschlecht-Verteilung---------------------------
gender_tab <- dat %>%
  dplyr::count(STRGESCHLECHT, name = "n") %>%
  dplyr::mutate(pct = n / sum(n))

ctx <<- tc_write_table(
  tab = gender_tab,
  table_id = "01_05_gender_distribution",
  ctx = ctx,
  threshold = TRUE,
  threshold_label_cols = c("STRGESCHLECHT")
)

## Grafik: Geschlecht---------------------------------------
p_gender <- ggplot2::ggplot(
  gender_tab,
  ggplot2::aes(x = STRGESCHLECHT, y = n)
) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Geschlecht", x = NULL, y = "Anzahl")

ctx <<- tc_write_figure(
  plot = p_gender,
  fig_id = "01_05_gender",
  filename = "gender_bar",
  ctx = ctx,
  threshold = TRUE,
  threshold_tab = gender_tab,
  threshold_label_cols = c("STRGESCHLECHT")
)
