# 03_01_02_Distance_Groups.R -----------
# Zweck: Distanzgruppen fuer wohnortnahe Versorgung

distance_group_tab <- tumor_base |>
  dplyr::mutate(distance_group = factor(as.character(distance_group), levels = config$analysis$distance_labels)) |>
  dplyr::group_by(distance_group) |>
  dplyr::summarise(
    n_tumors = dplyr::n_distinct(LNGTUMORID),
    n_patients = dplyr::n_distinct(LNGREGISTERNR),
    .groups = "drop"
  ) |>
  tidyr::complete(
    distance_group = factor(config$analysis$distance_labels, levels = config$analysis$distance_labels),
    fill = list(n_tumors = 0L, n_patients = 0L)
  ) |>
  dplyr::mutate(pct_tumors = safe_divide(n_tumors, sum(n_tumors)))

ctx <- tc_write_table(
  tab = distance_group_tab,
  table_id = "03_02_distance_groups",
  ctx = ctx,
  output_category = "aggregate_table",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("distance_group")
)

p_distance <- ggplot2::ggplot(distance_group_tab, ggplot2::aes(x = distance_group, y = n_tumors)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Tumoren nach Distanzgruppe", x = "Distanz", y = "Anzahl Tumoren") +
  ggplot2::theme_minimal()

ctx <- tc_write_figure(
  plot = p_distance,
  fig_id = "03_02_distance_groups",
  filename = "bar",
  ctx = ctx,
  threshold_tab = distance_group_tab,
  output_category = "aggregate_figure",
  threshold_n_col = c("n_tumors", "n_patients"),
  threshold_label_cols = c("distance_group")
)
