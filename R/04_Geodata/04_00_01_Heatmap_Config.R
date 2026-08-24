# Heatmap-Konfiguration -----------------------------
## Allgemeine Karteneinstellungen --------------------------

config$geodata$heatmap <- list(
  gkz_digits = 5L,
  allowed_gkz_digits = c(
    5L,
    8L
  ),
  simplify_tolerance_m = 200,
  line_width_by_digits = c(
    "5" = 0.15,
    "8" = 0.025
  ),
  city_point_size = 2.4,
  city_label_nudge_y = 5500,
  cities = tibble::tribble(
    ~gkz_join,  ~city_label,
    "08111000", "Stuttgart",
    "08311000", "Freiburg",
    "08416041", "Tuebingen"
  )
)


## Konfiguration pruefen --------------------------

stopifnot(
  config$geodata$heatmap$gkz_digits %in%
    config$geodata$heatmap$allowed_gkz_digits
)