# 01_01_03_Missingness.R

# Fallback, falls Reihenfolge mal gebrochen wird
if (!exists("variables_inventory", inherits = TRUE)) {
  variables_inventory <- dplyr::tibble(variable = names(dat)) %>%
    dplyr::mutate(
      n      = nrow(dat),
      n_na   = vapply(dat, function(x) sum(is.na(x)), integer(1)),
      pct_na = n_na / n
    ) %>%
    dplyr::arrange(dplyr::desc(pct_na), variable)
}

missing_plot_dat <- variables_inventory %>%
  dplyr::mutate(variable = stats::reorder(variable, pct_na))

p_missing <- ggplot2::ggplot(missing_plot_dat, ggplot2::aes(x = pct_na, y = variable)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Missingness pro Variable",
    x = "Missing-Anteil",
    y = NULL
  )

# Optional: dynamische Höhe, überschreibt config$figures$height nur hier
dyn_height <- max(config$figures$height, 0.18 * nrow(missing_plot_dat))

explore_write_figure(
  plot = p_missing,
  fig_id = "01_03_missingness",
  filename = "missingness_bar",
  paths = paths,
  log_file = log_file,
  height = dyn_height
)
