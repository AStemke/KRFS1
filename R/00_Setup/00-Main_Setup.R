# 00-Main_Setup
# Dieses Dokument lädt alle auszuführenden Setup Skripte und defniert globale Variablen.


# Setup -------------------------------------------------------------------
# Hier erfolgt das Sourcen der Überabschnitte, sowie die Definition globaler Variablen.
# Zentrale Konfiguration

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
  )
)

# Ableitungen ------------------------------------------------------------

config$paths$current_wd <- config$paths$osp_wd
setwd(config$paths$current_wd)
#TODO: setwd(config$paths$ #TODO)



# Sourcen der auszuführenden Skripte --------------------------------------

source(file.path("R", "00_Setup", "00_00-Libraries.R"))
source(file.path("R", "00_Setup", "00_01-Helper_Setup.R"))
source(file.path("R", "00_Setup", "00_02-Data_Setup.R"))
source(file.path("R", "01_Explore", "01_00-Explore_Setup.R"))
