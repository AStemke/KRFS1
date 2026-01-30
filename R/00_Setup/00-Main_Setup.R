# 00-Main_Setup
# Dieses Dokument lädt alle auszuführenden Setup Skripte und defniert globale Variablen.


# Setup -------------------------------------------------------------------
# Hier erfolgt das Sourcen der Überabschnitte, sowie die Definition globaler Variablen.
# Zentrale Konfiguration
source(file.path("R", "00_Setup", "00_00-Config.R"))

# Ableitungen ------------------------------------------------------------

config$paths$current_wd <- config$paths$osp_wd
setwd(config$paths$current_wd)
#TODO: setwd(config$paths$ #TODO)



# Sourcen der auszuführenden Skripte --------------------------------------

source(file.path("R", "00_Setup", "00_00-Libraries.R"))
source(file.path("R", "00_Setup", "00_01-Helper_Setup.R"))
source(file.path("R", "00_Setup", "00_02-Data_Setup.R"))
source(file.path("R", "01_Explore", "01_00-Explore_Setup.R"))
source(file.path("R", "01_Explore", "01_01-Explore_Sourcing.R"))
