# 01_01_08_Distance
# Zweck: Distanz-Analyse (keine Datenschutz-Schwelle erforderlich)

# Tabelle: Distanz Summary---------------------------
dist_summary <- dplyr::tibble(
  n_total = nrow(dat),
  n_na    = sum(is.na(dat$Distanz_km)),
  min_km  = min(dat$Distanz_km, na.rm = TRUE),
  median  = stats::median(dat$Distanz_km, na.rm = TRUE),
  mean    = mean(dat$Distanz_km, na.rm = TRUE),
  max_km  = max(dat$Distanz_km, na.rm = TRUE)
)

ctx <<- tc_write_table(
  tab = dist_summary,
  table_id = "01_08_distance_summary",
  ctx = ctx,
  threshold = FALSE
)

## Grafik: Distanz Histogramm-------------------------------
p_dist <- ggplot2::ggplot(
  dat,
  ggplot2::aes(x = Distanz_km)
) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(title = "Distanz (km)", x = "km", y = "Anzahl")

ctx <<- tc_write_figure(
  plot = p_dist,
  fig_id = "01_08_distance",
  filename = "distance_hist",
  ctx = ctx,
  threshold = FALSE
)
