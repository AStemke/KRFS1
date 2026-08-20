library(klr)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(forcats)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)
library(broom)
library(sf)
library(writexl)


# 00-Main_Setup.R -----------
# Zweck:
# - Zentrales Startskript fuer KRFS1 / Wohnortnahe Versorgung
# - Laedt Konfiguration, Pakete, Daten, Vorbereitung und Auswertungsskripte
# - Die Reihenfolge folgt der README-Logik: XX_YY_ZZ_Namensgebend.R

# Setup -----------
source(file.path("R", "00_Setup", "00_00-Config.R"))

# Arbeitsverzeichnis robust setzen -----------
# Im Produktivprojekt soll config$paths$osp_wd genutzt werden.
# Falls der Pfad lokal nicht existiert, wird das aktuelle Verzeichnis genutzt.
if (is.null(config$paths$current_wd)) {
  if (dir.exists(config$paths$osp_wd)) {
    config$paths$current_wd <- config$paths$osp_wd
  } else {
    config$paths$current_wd <- getwd()
    warning(
      "config$paths$osp_wd existiert lokal nicht. Nutze getwd(): ",
      config$paths$current_wd
    )
  }
}
setwd(config$paths$current_wd)

# 00-Main_Setup.R ---------------------------------------------------------

# Zentrale Konfiguration
source(file.path("R", "00_Setup", "00_00-Config.R"))


# Hilfsfunktion: NULL-Fallback -------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}


# Projekt-/Skriptpfad bestimmen ------------------------------------------
# Idee:
# - productive == TRUE  -> Partner-/OSP-Logik mit Systempfad() und Pfad
# - productive == FALSE -> lokale Dummy-/Entwicklungsumgebung, also getwd()

get_r_root <- function() {
  
  if (isTRUE(config$data$productive)) {
    
    #TODO: Bitte gerne eure Funktion einfügen oder so.
    r_root <- file.path(Systempfad(), Pfad)
    
  } else {
    
    # Dummy-/Entwicklungsumgebung:
    # Voraussetzung: getwd() ist der KRFS1-Projektordner
    r_root <- file.path(getwd(), "R")
  }
  
  r_root <- normalizePath(r_root, winslash = "/", mustWork = FALSE)
  
  if (!dir.exists(r_root)) {
    stop(
      "R-Skriptordner nicht gefunden:\n",
      r_root,
      "\n\nAktuelles Working Directory:\n",
      getwd(),
      "\n\nPruefe bitte config$data$productive, Systempfad(), Pfad und getwd()."
    )
  }
  
  r_root
}


# Zentrale Source-Funktion ------------------------------------------------
source_krfs1 <- function(...) {
  source(file.path(get_r_root(), ...))
}

# Setup-Skripte -----------------------------------------------------------
source_krfs1("00_Setup", "00_00-Libraries.R")
source_krfs1("00_Setup", "00_01-Helper_Setup.R")
source_krfs1("00_Setup", "00_02-Data_Setup.R")


# Datenaufbereitung -------------------------------------------------------
source_krfs1("01_Prepare", "01_00-Prepare_Setup.R")
source_krfs1("01_Prepare", "01_01-Prepare_Sourcing.R")


# Deskriptive Auswertungen ------------------------------------------------
source_krfs1("02_Descriptive", "02_00-Descriptive_Setup.R")
source_krfs1("02_Descriptive", "02_01-Descriptive_Sourcing.R")


# Wohnortnahe Versorgung / Distanz ---------------------------------------
source_krfs1("03_Access", "03_00-Access_Setup.R")
source_krfs1("03_Access", "03_01-Access_Sourcing.R")


# Geodaten / Karten -------------------------------------------------------
source_krfs1("04_Geodata", "04_00-Geodata_Setup.R")
source_krfs1("04_Geodata", "04_01-Geodata_Sourcing.R")


# Statistische Tests / Modelle -------------------------------------------
source_krfs1("05_Stats", "05_00-Stats_Setup.R")
source_krfs1("05_Stats", "05_01-Stats_Sourcing.R")


