# 00-Main_Setup
# Dieses Dokument lädt alle auszuführenden Setup Skripte und defniert globale Variablen.


# Setup -------------------------------------------------------------------
# Hier erfolgt das Sourcen der Überabschnitte, sowie die Definition globaler Variablen.

## Working  Directory ----------------------------------------------------

#Bitte auf das entsprechende Arbeitsverzeichnis setzen

osp_wd <- "T:/OSP/OSP_Projekte/Biometrie (neu)/Forschung/Kooperationen/2504_Krebsregister_Fragestellung_1_Wohnortnahe_Versorgung/Analyse/KRFS1"
#TODO: Ihr wd (working directory) einsetzen und entsprechende Zuweisung vornehmen

current_wd <- osp_wd
setwd(current_wd)


## Dataset ---------------------------------------------------------------
# Für Dummy Datensatz FALSE, für Productive TRUE
productive <- FALSE


## n-Threshold -----------------------------------------------------------
threshhold_n <- 3


# Sourcen der auszuführenden Skripte --------------------------------------

source(file.path("R", "00_00-Libraries.R"))
source(file.path("R", "00_01-Setup_Helper.R"))