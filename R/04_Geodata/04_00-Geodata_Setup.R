# Geodaten-Konfiguration -----------------------------
## Zweck --------------------------
# Zentrale Einstellungen fuer alle Geodatenauswertungen. Allgemeine
# Geodatenpfade und CRS-Angaben bleiben in 00_00-Config.R; Einstellungen,
# die nur die Karten des Analysemoduls betreffen, werden hier gebuendelt.

# Krebszentrumsstaedte -----------------------------
## Referenz --------------------------
# Stadtmarker fuer die Fokus-Entitaeten. Die Standorte innerhalb GKZ 0811
# wurden anhand des OnkoZert-Jahresberichts Onkologische Zentren 2025 fuer
# Darm, haematologische Neoplasien, Brust und Prostata vervollstaendigt.
# C18 und C20 werden dabei gemeinsam durch die Kategorie Darm abgebildet.
# Transfer-/Transit-Standorte werden nicht als Zentrum der Entitaet gewertet.
# Auf den Karten wird nur der Stadtname dargestellt; der Klinikname bleibt
# hier als dokumentierende Referenz erhalten.
center_cities_all <- tibble::tribble(
  ~gkz_join,  ~city_label,             ~center_name,                                      ~icd10_3,
  "08111000", "Stuttgart",            "Klinikum Stuttgart, Katharinenhospital",          "C18;C20;C50;C61;C83",
  "08115003", "Boeblingen",            "Klinikverbund Suedwest Kliniken Boeblingen",       "C18;C20;C50",
  "08115045", "Sindelfingen",         "Klinikverbund Suedwest Kliniken Sindelfingen",    "C61;C83",
  "08116019", "Esslingen",            "Klinikum Esslingen",                              "C18;C20;C50;C83",
  "08116080", "Ostfildern",           "medius KLINIK OSTFILDERN-RUIT",                    "C18;C20;C50;C61",
  "08117026", "Goeppingen",           "ALB FILS KLINIKUM Goeppingen",                     "C18;C20;C50;C61",
  "08118048", "Ludwigsburg",          "Klinikum Ludwigsburg",                            "C18;C20;C50;C61",
  "08119085", "Winnenden",            "Rems-Murr-Klinikum Winnenden",                     "C18;C20;C50;C61;C83",
  "08311000", "Freiburg",             "Universitaet Freiburg",                           "C18;C20;C50;C61;C83",
  "08221000", "Heidelberg",           "Universitaet Heidelberg",                         "C18;C20;C50;C61",
  "08222000", "Mannheim",             "Universitaet Mannheim",                           "C18;C20;C50;C61",
  "08416041", "Tuebingen",            "Universitaet Tuebingen",                          "C18;C20;C50;C61;C83",
  "08421000", "Ulm",                  "Universitaet Ulm",                                "C18;C20;C50;C61;C83",
  "08212000", "Karlsruhe",            "Staedtisches Klinikum Karlsruhe",                  "C18;C20;C50;C61;C83",
  "08136065", "Schwaebisch Gmuend",   "Stauferklinikum Schwaebisch Gmuend",              "C18;C20;C50;C83",
  "08231000", "Pforzheim",             "Siloah St. Trudpert Klinikum",                    "C18;C20;C50;C61"
)

## Aktive Entitaeten --------------------------
active_center_icd10_3 <- intersect(
  unique(toupper(config$analysis$active_icd10_3)),
  toupper(config$analysis$focus_icd10_3)
)

center_city_keep <- if (length(active_center_icd10_3) == 0L) {
  rep(FALSE, nrow(center_cities_all))
} else {
  vapply(
    strsplit(center_cities_all$icd10_3, ";", fixed = TRUE),
    function(x) any(toupper(x) %in% active_center_icd10_3),
    logical(1)
  )
}

center_cities_active <- center_cities_all |>
  dplyr::filter(center_city_keep) |>
  dplyr::distinct(gkz_join, city_label)

# Heatmap-Konfiguration -----------------------------
## Kartenvarianten --------------------------
config$geodata$heatmap <- list(
  output_groups = c(
    "Selektiv_Gesamt",
    config$analysis$focus_icd10_3
  ),

  # Posterfokus: ausschliesslich Raum 0811 ausgeben.
  # Die BW-Konfiguration bleibt fuer spaetere Auswertungen erhalten.
  active_maps = "stuttgart",

  # Baden-Wuerttemberg: bisherige Darstellung auf Kreisebene.
  bw = list(
    gkz_prefix = "08",
    gkz_digits = 5L,
    label = "Baden-Wuerttemberg"
  ),

  # GKZ 0811 entspricht administrativ der Region Stuttgart. Fuer das Poster
  # wird gemaess Projektkonvention die Bezeichnung Metropolregion Stuttgart genutzt.
  stuttgart = list(
    gkz_prefix = "0811",
    gkz_digits = 8L,
    label = "Metropolregion Stuttgart"
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

  # Einheitliche Darstellung der Krebszentrumsstaedte.
  city_point_size = 2.4,
  city_point_stroke = 0.6,
  city_label_size = 2.8,
  city_label_nudge_y = 5500,
  city_label_nudge_y_stuttgart = 900,

  # Alle dokumentierten Zentren sowie die fuer die aktive Entitaet relevanten
  # Stadtmarker. Dadurch zeigt jede Fokuskarte nur passende Zentren; die
  # Selektiv-Gesamt-Karte zeigt die Vereinigungsmenge.
  center_cities = center_cities_all,
  cities = center_cities_active
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
    c("gkz_join", "city_label", "center_name", "icd10_3") %in%
      names(config$geodata$heatmap$center_cities)
  ),
  all(
    c("gkz_join", "city_label") %in%
      names(config$geodata$heatmap$cities)
  )
)

# Bestehende BW-Heatmap synchronisieren -----------------------------
## Rueckwaertskompatiblen Kartenlevel setzen --------------------------
config$geodata$heatmap$gkz_digits <- config$geodata$heatmap$bw$gkz_digits
