# Geodaten-Konfiguration -----------------------------
## Zweck --------------------------
# Zentrale Einstellungen fuer alle Geodatenauswertungen. Allgemeine
# Geodatenpfade und CRS-Angaben bleiben in 00_00-Config.R; fachliche
# Kartenparameter liegen hier, visuelle Einstellungen in config$figures$maps.

# Krebszentrumsstaedte -----------------------------
## Referenz --------------------------
# Stadtmarker fuer die Fokus-Entitaeten. Die Standorte innerhalb GKZ 0811
# wurden anhand des OnkoZert-Jahresberichts und aktueller Klinikangaben fuer
# Darm, haematologische Neoplasien, Brust und Prostata vervollstaendigt.
# C18 und C20 werden dabei gemeinsam durch die Kategorie Darm abgebildet.
# Transfer-/Transit-Standorte werden nicht als Zentrum der Entitaet gewertet.
# Bei standortuebergreifenden zertifizierten Zentren werden die beteiligten
# Klinikstandorte aufgenommen. Auf den Karten wird nur der Stadtname dargestellt;
# der Klinikname bleibt hier als dokumentierende Referenz erhalten.
center_cities_all <- tibble::tribble(
  ~gkz_join,  ~city_label,             ~center_name,                                      ~icd10_3,
  "08111000", "Stuttgart",            "Klinikum Stuttgart, Katharinenhospital",          "C18;C20;C50;C61;C83",
  "08115003", "Boeblingen",            "Klinikverbund Suedwest Kliniken Boeblingen",       "C18;C20;C50",
  "08115045", "Sindelfingen",         "Klinikverbund Suedwest Kliniken Sindelfingen",    "C61;C83",
  "08116019", "Esslingen",            "Klinikum Esslingen",                              "C18;C20;C50;C83",
  "08116049", "Nuertingen",            "medius KLINIK NUERTINGEN",                         "C18;C20;C50",
  "08116080", "Ostfildern",           "medius KLINIK OSTFILDERN-RUIT",                    "C18;C20;C50;C61",
  "08117026", "Goeppingen",           "ALB FILS KLINIKUM Goeppingen",                     "C18;C20;C50;C61",
  "08118048", "Ludwigsburg",          "Klinikum Ludwigsburg",                            "C18;C20;C50;C61",
  "08118079", "Bietigheim-Bissingen",  "RKH Krankenhaus Bietigheim-Vaihingen",             "C18;C20;C50",
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
## Aktive Wohnregion --------------------------
analysis_region <- config$analysis$analysis_region
analysis_region_enabled <- !is.null(analysis_region) && isTRUE(analysis_region$enabled)
figure_map_config <- config$figures$maps

## Kartenvarianten --------------------------
config$geodata$heatmap <- list(
  output_groups = c(
    "Selektiv_Gesamt",
    config$analysis$focus_icd10_3
  ),

  # Bei aktiviertem Wohnortfilter wird die regionale Detailkarte erzeugt.
  # Wird der Filter deaktiviert, steht wieder die BW-Karte im Vordergrund.
  active_maps = if (analysis_region_enabled) "stuttgart" else "bw",

  # Baden-Wuerttemberg: bisherige Darstellung auf Kreisebene.
  bw = list(
    gkz_prefix = "08",
    gkz_digits = 5L,
    label = "Baden-Wuerttemberg"
  ),

  # Regionale Detailkarte. Prefix und Label folgen derselben zentralen
  # Wohnortkonfiguration wie Tabellen, Grafiken und Modelle.
  stuttgart = list(
    gkz_prefix = if (analysis_region_enabled) analysis_region$gkz_prefix else "0811",
    gkz_digits = 8L,
    label = if (analysis_region_enabled) analysis_region$label else "Stadtkreis Stuttgart und Umgebung"
  ),

  # Rueckwaertskompatible Parameter fuer die bestehende BW-Heatmap.
  gkz_digits = 5L,
  allowed_gkz_digits = c(5L, 8L),
  # Geringere Vereinfachung erhaelt gemeinsame Gemeindegrenzen genauer und
  # reduziert sichtbare Rasterartefakte zwischen benachbarten Polygonen.
  simplify_tolerance_m = 50,

  # Visuelle Kartenparameter kommen aus der zentralen Grafik-Konfiguration.
  boundary_color = figure_map_config$boundary_color,
  line_width_by_digits = figure_map_config$line_width_by_digits,
  city_point_size = figure_map_config$city_point_size,
  city_point_stroke = figure_map_config$city_point_stroke,
  city_label_size = figure_map_config$city_label_size,
  city_label_nudge_y = figure_map_config$city_label_nudge_y,
  city_label_nudge_y_stuttgart = figure_map_config$city_label_nudge_y_stuttgart,

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
