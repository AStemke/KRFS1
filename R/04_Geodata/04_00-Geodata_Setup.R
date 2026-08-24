# 04_00-Geodata_Setup.R
#
# Zentrale Einstellungen fuer alle Geodatenauswertungen. Allgemeine
# Geodatenpfade und CRS-Angaben bleiben in 00_00-Config.R; Einstellungen,
# die nur die Karten des Analysemoduls betreffen, werden hier gebuendelt.


# Heatmap-Konfiguration -----------------------------
## Kartenebene und Geometrie --------------------------

config$geodata$heatmap <- list(
  output_groups = c(
    "Selektiv_Gesamt",
    config$analysis$focus_icd10_3
  ),
  gkz_digits = 5L,
  allowed_gkz_digits = c(
    5L,
    8L
  ),
  simplify_tolerance_m = 200,
  boundary_color = "grey70",
  line_width_by_digits = c(
    "5" = 0.15,
    "8" = 0.025
  ),
  city_point_size = 2.4,
  city_point_stroke = 0.6,
  city_label_size = 3.2,
  city_label_nudge_y = 5500,
  cities = tibble::tribble(
    ~gkz_join,  ~city_label,
    "08111000", "Stuttgart",
    "08311000", "Freiburg",
    "08416041", "Tuebingen"
  )
)


## Plausibilitaetspruefung --------------------------

stopifnot(
  config$geodata$heatmap$gkz_digits %in%
    config$geodata$heatmap$allowed_gkz_digits,
  all(
    as.character(
      config$geodata$heatmap$allowed_gkz_digits
    ) %in%
      names(
        config$geodata$heatmap$line_width_by_digits
      )
  ),
  all(
    c(
      "gkz_join",
      "city_label"
    ) %in%
      names(
        config$geodata$heatmap$cities
      )
  )
)
