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
  # Für Dummy Datensatz FALSE, für Productive TRUE
  data = list(
    productive = FALSE
  ),
  
  ## n-Threshold ----------------------------------------------------------- 
  thresholds = list(
    n_min = 3
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
  )
)
