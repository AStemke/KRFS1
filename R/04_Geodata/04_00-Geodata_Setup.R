# 04_00-Geodata_Setup.R -----------------------------
## Zweck --------------------------
# Zentrale Einstellungen fuer alle Geodatenauswertungen. Allgemeine
# Geodatenpfade und CRS-Angaben bleiben in 00_00-Config.R; Einstellungen,
# die nur die Karten des Analysemoduls betreffen, werden hier gebuendelt.

# Heatmap-Konfiguration -----------------------------
## Kartenvarianten --------------------------
config$geodata$heatmap <- list(
  output_groups = c(
    "Selektiv_Gesamt",
    config$analysis$focus_icd10_3
  ),

  # Moegliche Werte: "bw" und "stuttgart".
  # Es kann eine oder beide Kartenvarianten ausgegeben werden.
  active_maps = c("bw", "stuttgart"),

  # Baden-Wuerttemberg: bisherige Darstellung auf Kreisebene.
  bw = list(
    gkz_prefix = "08",
    gkz_digits = 5L,
    label = "Baden-Wuerttemberg"
  ),

  # Stuttgart: nur Diagnose-GKZ mit Prefix 0811 und eine Ebene feiner.
  # Mit der derzeitigen Gemeindedatei bedeutet dies die achtstellige GKZ-Ebene.
  stuttgart = list(
    gkz_prefix = "0811",
    gkz_digits = 8L,
    label = "GKZ 0811"
  ),

  # Rueckwaertskompatible Parameter fuer die bestehende BW-Heatmap.
  gkz_digits = 5L,
  allowed_gkz_digits = c(5L, 8L),
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

# Plausibilitaetspruefung -----------------------------
## Kartenparameter --------------------------
stopifnot(
  all(config$geodata$heatmap$active_maps %in% c("bw", "stuttgart")),
  config$geodata$heatmap$bw$gkz_digits %in%
    config$geodata$heatmap$allowed_gkz_digits,
  config$geodata$heatmap$stuttgart$gkz_digits %in%
    config$geodata$heatmap$allowed_gkz_digits,
  all(
    as.character(config$geodata$heatmap$allowed_gkz_digits) %in%
      names(config$geodata$heatmap$line_width_by_digits)
  ),
  all(
    c("gkz_join", "city_label") %in%
      names(config$geodata$heatmap$cities)
  )
)

# Bestehende BW-Heatmap synchronisieren -----------------------------
## Rueckwaertskompatiblen Kartenlevel setzen --------------------------
config$geodata$heatmap$gkz_digits <- config$geodata$heatmap$bw$gkz_digits
