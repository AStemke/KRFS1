# 01_01_03_Missingness
# Zweck: Visualisierung der Missingness pro Variable

# Plot: Missingness-----------------------------------------
miss_plot_dat <- variables_inventory %>%
  dplyr::mutate(variable = stats::reorder(variable, pct_na))

p_missing <- ggplot2::ggplot(
  miss_plot_dat,
  ggplot2::aes(x = pct_na, y = variable)
) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Missingness pro Variable",
    x = "Anteil fehlend",
    y = NULL
  )

ctx <<- tc_write_figure(
  plot = p_missing,
  fig_id = "01_03_missingness",
  filename = "missingness_bar",
  ctx = ctx,
  threshold = FALSE
)
