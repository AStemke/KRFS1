# Main Setup -----------------------------
## Zweck --------------------------
# Zentrales Startskript fuer KRFS1 / Wohnortnahe Versorgung.
# Laedt Konfiguration, Pakete, Daten, Vorbereitung und Auswertungsskripte.
# Die Reihenfolge folgt der README-Logik: XX_YY_ZZ_Namensgebend.R.

# Konfiguration -----------------------------
## Zentrale Konfiguration laden --------------------------
source(file.path("R", "00_Setup", "00_00-Config.R"))

## Arbeitsverzeichnis festlegen --------------------------
# Wenn current_wd nicht explizit gesetzt ist, wird bevorzugt der OSP-Projektpfad genutzt.
# Existiert dieser lokal nicht, wird das aktuelle Working Directory verwendet.
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

stopifnot(
  is.character(config$paths$current_wd),
  length(config$paths$current_wd) == 1,
  nzchar(config$paths$current_wd)
)

setwd(config$paths$current_wd)

# Source-Funktionen -----------------------------
## R-Skriptordner bestimmen --------------------------
# productive == TRUE:
#   Partner-/Produktivlogik mit Systempfad() und Pfad.
# productive == FALSE:
#   R-Unterordner des aktuellen KRFS1-Projektordners.
get_r_root <- function() {
  if (isTRUE(config$data$productive)) {
    if (!exists("Systempfad", mode = "function")) {
      stop("Funktion `Systempfad()` wurde fuer die Produktivumgebung nicht gefunden.")
    }
    if (!exists("Pfad", inherits = TRUE)) {
      stop("Objekt `Pfad` wurde fuer die Produktivumgebung nicht gefunden.")
    }

    r_root <- file.path(Systempfad(), Pfad)
  } else {
    r_root <- file.path(config$paths$current_wd, "R")
  }

  r_root <- normalizePath(r_root, winslash = "/", mustWork = FALSE)

  if (!dir.exists(r_root)) {
    stop(
      "R-Skriptordner nicht gefunden:\n",
      r_root,
      "\n\nAktuelles Working Directory:\n",
      getwd(),
      "\n\nPruefe bitte config$data$productive, config$paths$current_wd, ",
      "Systempfad() und Pfad."
    )
  }

  r_root
}

## Zentrale Source-Funktion --------------------------
source_krfs1 <- function(...) {
  source(file.path(get_r_root(), ...))
}

# Setup -----------------------------
## Pakete, Helper und Daten --------------------------
source_krfs1("00_Setup", "00_00-Libraries.R")
source_krfs1("00_Setup", "00_01-Helper_Setup.R")
source_krfs1("00_Setup", "00_02-Data_Setup.R")

# Datenaufbereitung -----------------------------
## Prepare --------------------------
source_krfs1("01_Prepare", "01_00-Prepare_Setup.R")
source_krfs1("01_Prepare", "01_01-Prepare_Sourcing.R")

# Deskriptive Auswertungen -----------------------------
## Descriptive --------------------------
source_krfs1("02_Descriptive", "02_00-Descriptive_Setup.R")
source_krfs1("02_Descriptive", "02_01-Descriptive_Sourcing.R")

# Wohnortnahe Versorgung -----------------------------
## Distanz --------------------------
source_krfs1("03_Access", "03_00-Access_Setup.R")
source_krfs1("03_Access", "03_01-Access_Sourcing.R")

# Geodaten -----------------------------
## Karten --------------------------
source_krfs1("04_Geodata", "04_00-Geodata_Setup.R")
source_krfs1("04_Geodata", "04_01-Geodata_Sourcing.R")

# Statistik -----------------------------
## Tests und Modelle --------------------------
source_krfs1("05_Stats", "05_00-Stats_Setup.R")
source_krfs1("05_Stats", "05_01-Stats_Sourcing.R")
