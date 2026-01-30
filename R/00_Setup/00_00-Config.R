# 00_00-Config.R

config <- list(
  
  ## Working  Directory ----------------------------------------------------
  #Bitte auf das entsprechende Arbeitsverzeichnis setzen
  #TODO: Ihr wd (working directory) einsetzen und entsprechende Zuweisung vornehmen
  paths = list(
    osp_wd     = "T:/OSP/OSP_Projekte/Biometrie (neu)/Forschung/Kooperationen/2504_Krebsregister_Fragestellung_1_Wohnortnahe_Versorgung/Analyse/KRFS1",
    current_wd = NULL   # wird unten gesetzt
  ),
  
  ## Dataset ---------------------------------------------------------------
  #TODO: Für Dummy Datensatz FALSE, für Productive TRUE
  data = list(
    productive = FALSE
  ),
  
  ## n-Threshold ----------------------------------------------------------- 
  thresholds = list(
    n_min = 3,
    
    # Auto-Detection: nur Spalten, die wie Zählspalten aussehen
    # Empfehlung: Namenskonvention "n", "n_*", "N", "N_*"
    auto_regex = "^(n|N)",
    
    # Auto-Detection: nur ganzzahlige Werte (keine Nachkommastellen)
    auto_require_integerish = TRUE,
    
    # Auto-Detection: Ausschlussliste, falls es numerische "n-ähnliche" Spalten gibt,
    # die KEINE Fallzahlen sind
    auto_exclude = character(0)
  ),
  
  ## figure settings ------------------------------------------------------- 
  figures = list(
    width  = 10,
    height = 6,
    dpi    = 300
  ),
  
  ## Mandatory Variables ---------------------------------------------------  
  mandatory_vars = c(
    "LNGTUMORID",
    "LNGREGISTERNR",
    "DIAGNOSEDATUM",
    "STRGESCHLECHT",
    "STRDIAGNOSEICD10"
  ),
  
  explore = list(
    age_breaks = c(
      -Inf, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, Inf),
    age_labels = c(
      "<18", "18-<25", "25-<30", "30-<35", "35-<40", "40-<45", "45-<50", "50-<55",
      "55-<60", "60-<65", "65-<70", "70-<75", "75-<80", "80-<85", "85-<90", "90-<95", ">=95"
    )
  )
)
