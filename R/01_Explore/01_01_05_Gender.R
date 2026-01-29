# 01_01_05_Gender.R

gender_tab <- dat %>%
  dplyr::count(STRGESCHLECHT, name = "n") %>%
  dplyr::mutate(pct = n / sum(n))

excel_tabs <<- explore_write_table(
  tab = gender_tab,
  table_id = "01_05_gender_distribution",
  paths = paths,
  excel_list = excel_tabs,
  log_file = log_file
)

p_gender <- ggplot2::ggplot(gender_tab, ggplot2::aes(x = STRGESCHLECHT, y = n)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Geschlecht", x = NULL, y = "Anzahl")

explore_write_figure(
  plot = p_gender,
  fig_id = "01_05_gender",
  filename = "gender_bar",
  paths = paths,
  log_file = log_file
)
