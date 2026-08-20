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
    gkz_path = file.path("data", "geodata", "gemeinden.gpkg"),
    gkz_layer = NULL,
    gkz_key_sf = "GKZ",
    plz_path = file.path("data", "geodata", "plz.gpkg"),
    plz_layer = NULL,
    plz_key_sf = "plz"
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
    focus_icd10_3 = c("C50", "C61", "C34", "C18", "C44"),

    # Wird im Main-Skript beim Durchlauf der Kohorten gesetzt.
    active_output_group = "Gesamt",
    active_icd10_3 = character(0),

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
  ## Altersklassen --------------------------
  explore = list(
    age_breaks = c(-Inf, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, Inf),
    age_labels = c("<18", "18-<25", "25-<30", "30-<35", "35-<40", "40-<45", "45-<50", "50-<55", "55-<60", "60-<65", "65-<70", "70-<75", "75-<80", "80-<85", "85-<90", "90-<95", ">=95")
  )
)
