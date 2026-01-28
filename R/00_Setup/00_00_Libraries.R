# 00_00_Libraries

# Dieses Skript beinhaltet sämtliche zum Setup notwendigen Libraries


# Setup -------------------------------------------------------------------

#Bitte auf das entsprechende Arbeitsverzeichnis setzen

osp_wd <- "T:/OSP/OSP_Projekte/Biometrie (neu)/Forschung/Kooperationen/2504_Krebsregister_Fragestellung_1_Wohnortnahe_Versorgung/Analyse/KRFS1"
#TODO: Ihr wd einsetzen und 

current_wd <- osp_wd
setwd(current_wd)



# Helper ------------------------------------------------------------------

load_pkgs <- function(pkgs, quietly = TRUE) {
  stopifnot(is.character(pkgs))
  
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE, quietly = quietly)) {
      message("Installiere Paket: ", pkg)
      install.packages(pkg, dependencies = TRUE)
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = quietly)
      )
    }
  }
  
  invisible(TRUE)
}


# Load Packages -----------------------------------------------------------

load_pkgs(c(
  "tidyverse",
  "survival",
  "mstate",
  "broom",
  "lubridate"
))


