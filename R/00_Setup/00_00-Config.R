# 00_00-Config.R -----------------------------
## Zweck --------------------------
# Zentrale Einstellungen fuer Pfade, Daten, Datenschutz, Geodaten und Grafiken.
# Fuer Dummy-Daten: config$data$productive <- FALSE
# Fuer Produktivdaten: config$data$productive <- TRUE

config <- list(
  # Working Directory -----------------------------
  ## Pfade --------------------------
  paths = list(
    osp_wd = "T:/OSP/OSP_Projekte/Biometrie (neu)/Forschung/Kooperationen/2504_Krebsregister_Fragestellung_1_Wohnortnahe_Versorgung/Analyse/KRFS1",
    kr_wd = "/mnt/krebsregister/Auswertungen/Versorgungsforschung/2025_OSP_Stuttgart/",
    current_wd = NULL
  ),

  # Pakete -----------------------------
  ## Installation --------------------------
  packages = list(
    # TRUE installiert fehlende Pakete automatisch.
    # Fuer reproduzierbare Umgebungen ggf. FALSE setzen und renv nutzen.
    install_missing = FALSE
  ),

  # Dataset -----------------------------
  ## Datenquelle --------------------------
  # TODO: productive auf TRUE setzen fuer Auswertung, Tests etc. auf FALSE.
  data = list(
    productive = FALSE,
    sheet_data = "Sheet 1",
    sheet_legend = "Legende",
    dummy_data_regex = "^Dummy_Datensatz_[0-9]{8}\\.xlsx$",
    dummy_legend_regex = "^Dummy_Datensatz_[0-9]{8}_Legende\\.xlsx$",
    productive_data_regex = "^Productive_Datensatz_[0-9]{8}\\.rds$",
    productive_legend_regex = "^Productive_Datensatz_[0-9]{8}_Legende\\.xlsx$",
    prefer_existing_ads = TRUE
  ),

  # Datenschutz-Thresholds -----------------------------
  ## Zellunterdrueckung --------------------------
  thresholds = list(
    # Regel: Jede ausgegebene Zellfallzahl muss entweder 0 oder >= n_min sein.
    n_min = 3,
    auto_regex = "^(n|N)",
    auto_require_integerish = TRUE,
    auto_exclude = character(0)
  ),

  # Output-Kategorien -----------------------------
  ## Threshold-Logik --------------------------
  output_categories = list(
    technical_overview = list(
      threshold = FALSE,
      n_col = character(0),
      description = "Technische Strukturchecks ohne sensible Zellfallzahlen"
    ),
    aggregate_table = list(
      threshold = TRUE,
      n_col = "auto",
      mode = "any",
      description = "Aggregierte Tabellen mit Zellfallzahlen"
    ),
    aggregate_figure = list(
      threshold = TRUE,
      n_col = "auto",
      mode = "any",
      description = "Grafiken auf Basis aggregierter Tabellen"
    ),
    descriptive_numeric = list(
      threshold = TRUE,
      n_col = c("n", "n_total", "n_non_missing", "n_patients", "n_tumors"),
      mode = "any",
      description = "Numerische Kennzahlen, z.B. Distanz-Summary je Gruppe"
    ),
    crosstab = list(
      threshold = TRUE,
      n_col = c("n", "n_patients", "n_tumors"),
      mode = "any",
      description = "Kreuztabellen; jede Zelle muss 0 oder >= n_min sein"
    ),
    map_area = list(
      threshold = TRUE,
      n_col = c("n", "n_patients", "n_tumors"),
      mode = "any",
      description = "Karten-/Gebietsauswertungen nach GKZ/PLZ/Region"
    ),
    statistical_test = list(
      threshold = TRUE,
      n_col = c("n", "n_total", "n_patients", "n_tumors"),
      mode = "any",
      description = "Testergebnisse nur bei safe zugrunde liegenden Gruppen"
    ),
    model_result = list(
      threshold = TRUE,
      n_col = c("n_model", "n_events", "n_nonevents", "n", "n_patients"),
      mode = "any",
      description = "Regressionsmodelle inkl. Mindestbesetzung von Modellframe/Leveln"
    )
  ),

  # Mandatory Variables -----------------------------
  ## Pflichtvariablen --------------------------
  mandatory_vars = c(
    "LNGTUMORID",
    "LNGREGISTERNR",
    "DIAGNOSEDATUM",
    "STRGESCHLECHT",
    "STRDIAGNOSEICD10",
    "GKZ_DIAGNOSE",
    "PLZ_DIAGNOSE",
    "PLZ_PATIENT",
    "PLZ_Krankenhaus",
    "Distanz_km"
  ),

  # Geodaten -----------------------------
  ## Kartenbasis --------------------------
  geodata = list(
    use_geodata = TRUE,
    # Erwartete Projektion fuer Baden-Wuerttemberg / metrische Distanzen.
    # EPSG:25832 = ETRS89 / UTM Zone 32N.
    crs_target = 25832,
    crs_lonlat = 4326,
    # Platzhalter: bitte an finale Datenquelle anpassen.
    gkz_path = file.path(
      "data",
      "Geodata",
      "raw",
      "verwaltungsgrenzen",
      "v_al_gemeinde.shp"
    ),
    gkz_layer = NULL,
    gkz_key_sf = "gemeinde_i"
  ),

  # Analyseparameter -----------------------------
  ## ICD-10-Kohorten und Distanz --------------------------
  analysis = list(
    diagnosis_year_min = NULL,
    diagnosis_year_max = NULL,
    icd10_prefix_keep = NULL,

    # Selektive Gesamtpopulation gemaess Projektbeschreibung.
    project_icd10_3 = c(
      "C18", "C19", "C20", "C25", "C50", "C56", "C61", "C81",
      paste0("C", 82:88),
      "C90",
      paste0("C", 91:95)
    ),

    # Entitaeten mit separaten Auswertungsordnern.
    focus_icd10_3 = c("C50", "C61", "C18", "C20", "C83"),

    # Entitaeten fuer die kompakte Ergebnis-Uebersicht.
    result_overview_icd10_3 = c("C50", "C61", "C18", "C20", "C83"),

    # Regionseinteilung fuer die Ergebnis-Uebersicht.
    # 0811 = Stuttgart; 08 = Baden-Wuerttemberg.
    result_overview_region = list(
      stuttgart_gkz_prefix = "0811",
      bw_gkz_prefix = "08",
      label_stuttgart = "Stuttgart",
      label_bw_other = "Baden-Wuerttemberg ohne Stuttgart",
      label_outside_bw = "Ausserhalb Baden-Wuerttembergs",
      label_missing = "GKZ fehlend"
    ),

    # Wird im Main-Skript beim Durchlauf der Kohorten gesetzt.
    active_output_group = "Gesamt",
    active_icd10_3 = character(0),

    # Default fuer Entitaeten ohne eigenen Grenzwert.
    near_care_default_km = 25,

    # Optionale entitaetsspezifische Grenzwerte. Weitere ICD-10-Codes koennen
    # einfach als benannte Werte ergaenzt werden, z.B. c(C50 = 20, C61 = 30).
    near_care_km_by_icd = c(),

    # Rueckwaertskompatibler Alias fuer Skripte, die nur einen Wert anzeigen.
    near_care_km = 25,

    distance_breaks_km = c(-Inf, 10, 25, 50, 100, Inf),
    distance_labels = c("<10 km", "10-<25 km", "25-<50 km", "50-<100 km", ">=100 km")
  ),

  # Figure Settings -----------------------------
  ## Groesse und Aufloesung --------------------------
  figures = list(
    width = 10,
    height = 6,
    dpi = 300
  ),

  # Explore / Altersgruppen -----------------------------
  ## Altersklassenschemata --------------------------
  explore = list(
    # Dieses Schema wird ausserhalb der Deskription verwendet und kann schnell
    # durch Aendern eines einzelnen Wertes gewechselt werden.
    active_age_scheme = "versorgung",

    # In der Deskription werden standardmaessig alle unten definierten Varianten
    # nebeneinander ausgegeben. FALSE beschraenkt auf active_age_scheme.
    run_all_age_schemes_descriptive = TRUE,

    age_schemes = list(
      fein = list(
        label = "Feine Altersklassen",
        breaks = c(-Inf, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, Inf),
        labels = c("<18", "18-<25", "25-<30", "30-<35", "35-<40", "40-<45", "45-<50", "50-<55", "55-<60", "60-<65", "65-<70", "70-<75", "75-<80", "80-<85", "85-<90", "90-<95", ">=95")
      ),
      versorgung = list(
        label = "Versorgungsorientierte Altersklassen",
        breaks = c(-Inf, 18, 50, 60, 70, 80, Inf),
        labels = c("<18", "18-<50", "50-<60", "60-<70", "70-<80", ">=80")
      ),
      geriatrisch = list(
        label = "Geriatrisch orientierte Altersklassen",
        breaks = c(-Inf, 18, 50, 65, 75, 85, Inf),
        labels = c("<18", "18-<50", "50-<65", "65-<75", "75-<85", ">=85")
      )
    )
  )
)

# Aktives Altersklassenschema -----------------------------
## Rueckwaertskompatible age_breaks und age_labels setzen --------------------------
active_age_scheme <- config$explore$active_age_scheme
if (!active_age_scheme %in% names(config$explore$age_schemes)) {
  stop(
    "Unbekanntes config$explore$active_age_scheme: ",
    active_age_scheme,
    call. = FALSE
  )
}

config$explore$age_breaks <- config$explore$age_schemes[[active_age_scheme]]$breaks
config$explore$age_labels <- config$explore$age_schemes[[active_age_scheme]]$labels

# Distanzkonfiguration -----------------------------
## Rueckwaertskompatiblen Default synchronisieren --------------------------
config$analysis$near_care_km <- config$analysis$near_care_default_km
