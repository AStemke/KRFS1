# 02_01_03_Demographics.R -----------
# Zweck: Altersgruppen und Geschlecht

demo_tab <- tumor_base |>
  dplyr::mutate(
    sex = dplyr::if_else(is.na(sex), "unbekannt", sex),
    age_group = factor(as.character(age_group), levels = config$explore$age_labels)
  ) |>
  dplyr::group_by(age_group, sex) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    .groups = "drop"
  ) |>
  tidyr::complete(
    age_group = factor(config$explore$age_labels, levels = config$explore$age_labels),
    sex,
    fill = list(n_tumors = 0L, n_patients = 0L)
  )

ctx <- tc_write_table(
  tab = demo_tab,
  table_id = "02_03_age_sex",
  ctx = ctx,
  output_category = "crosstab",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("age_group", "sex")
)

# Threshold fuer Grafik -----------
demo_plot_tab <- mask_threshold_category(
  tab = demo_tab,
  output_category = "aggregate_figure",
  n_col = c("n_tumors", "n_patients"),
  label_cols = c("age_group", "sex"),
  object_name = "02_03_age_sex_stacked_bar"
)$tab

p_demo <- ggplot2::ggplot(demo_plot_tab, ggplot2::aes(x = age_group, y = n_tumors, fill = sex)) +
  ggplot2::geom_col(position = "stack") +
  ggplot2::labs(title = "Tumoren nach Altersgruppe und Geschlecht", x = "Altersgruppe", y = "Anzahl Tumoren", fill = "Geschlecht") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ctx <- tc_write_figure(
  plot = p_demo,
  fig_id = "02_03_age_sex",
  filename = "stacked_bar",
  ctx = ctx,
  threshold_tab = demo_plot_tab,
  output_category = "aggregate_figure",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("age_group", "sex")
)
