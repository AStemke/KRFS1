# Main Setup -----------------------------
## Zweck --------------------------
# Zentrales Startskript fuer KRFS1 / Wohnortnahe Versorgung.
# Laedt Konfiguration, Pakete, Daten, Vorbereitung und Auswertungsskripte.
# Die Auswertungen werden fuer Gesamt, Selektiv_Gesamt sowie die Fokus-ICD-10-Codes ausgegeben.

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
## Gesamtpopulation vorbereiten --------------------------
source_krfs1("01_Prepare", "01_00-Prepare_Setup.R")
source_krfs1("01_Prepare", "01_01-Prepare_Sourcing.R")

# Analysebasen -----------------------------
## Ungefilterte Basis sichern --------------------------
tumor_base_all <- tumor_base
patient_base_all <- patient_base

# Analyse-Kohorten -----------------------------
## Output-Gruppen definieren --------------------------
focus_groups <- stats::setNames(
  lapply(config$analysis$focus_icd10_3, function(icd) icd),
  config$analysis$focus_icd10_3
)

analysis_groups <- c(
  list(
    Gesamt = character(0),
    Selektiv_Gesamt = config$analysis$project_icd10_3
  ),
  focus_groups
)

# Auswertungen -----------------------------
## Alle Analysebereiche je Kohorte durchlaufen --------------------------
for (group_name in names(analysis_groups)) {
  config$analysis$active_output_group <- group_name
  config$analysis$active_icd10_3 <- analysis_groups[[group_name]]

  message(
    "Starte Analysegruppe: ",
    group_name,
    if (length(config$analysis$active_icd10_3) > 0L) {
      paste0(" [", paste(config$analysis$active_icd10_3, collapse = ", "), "]")
    } else {
      " [alle ICD-10]"
    }
  )

  # Deskriptive Auswertungen -----------------------------
  ## Descriptive --------------------------
  source_krfs1("02_Descriptive", "02_00-Descriptive_Setup.R")
  source_krfs1("02_Descriptive", "02_01-Descriptive_Sourcing.R")

  # Wohnortnahe Versorgung -----------------------------
  ## Distanz --------------------------
  source_krfs1("03_Access", "03_00-Access_Setup.R")
  source_krfs1("03_Access", "03_01-Access_Sourcing.R")

  # Geodaten -----------------------------
  ## Karten und Heatmap --------------------------
  source_krfs1("04_Geodata", "04_00-Geodata_Setup.R")
  source_krfs1("04_Geodata", "04_01-Geodata_Sourcing.R")

  # Statistik -----------------------------
  ## Tests und Modelle --------------------------
  source_krfs1("05_Stats", "05_00-Stats_Setup.R")
  source_krfs1("05_Stats", "05_01-Stats_Sourcing.R")
}

# Abschluss -----------------------------
## Gesamtbasis als aktive Basis wiederherstellen --------------------------
config$analysis$active_output_group <- "Gesamt"
config$analysis$active_icd10_3 <- character(0)
tumor_base <- tumor_base_all
patient_base <- patient_base_all
