# 00_00-Libraries.R -----------
# Zweck:
# - Prueft alle benoetigten Pakete ohne sie global per library() anzuhängen
# - Alle Funktionsaufrufe erfolgen in den Skripten konsequent mit paket::funktion()

# Paketuebersicht -----------
# package: Paketname
# purpose: Warum das Paket im Projekt benoetigt wird
pkg_purpose <- data.frame(
  package = c(
    "readxl",
    "readr",
    "writexl",
    "dplyr",
    "tidyr",
    "tibble",
    "stringr",
    "forcats",
    "purrr",
    "lubridate",
    "ggplot2",
    "scales",
    "broom",
    "lme4",
    "sf",
    "units"
  ),
  purpose = c(
    "Excel-Rohdaten und Legenden aus data/*/raw einlesen",
    "CSV-Tabellen in output/*/tables schreiben",
    "Excel-Sammelmappen fuer Tabellenexporte schreiben",
    "Daten filtern, gruppieren, aggregieren und Variablen ableiten",
    "Kombinationen vervollstaendigen, z.B. 0-Zellen in Kreuztabellen sichtbar machen",
    "Tibbles fuer strukturierte Tabellenobjekte erzeugen",
    "Zeichenketten bearbeiten, z.B. ICD-10, GKZ, PLZ und Dateinamen",
    "Faktorlevel ordnen, z.B. Altersgruppen und Distanzgruppen",
    "Listenverarbeitung, z.B. sichere Mehrfachoutputs",
    "Datumswerte robust parsen und Diagnosejahre ableiten",
    "Grafiken fuer Balkendiagramme, Histogramme und Karten erzeugen",
    "Achsen und Prozentwerte in Grafiken formatieren",
    "Statistische Tests und Modelloutputs in Tabellenform bringen",
    "Logistische Mixed-Effects-Modelle fuer regionale Clusterung schaetzen",
    "Geodaten lesen, transformieren, verbinden und als Karten darstellen",
    "Einheiten fuer Geometrien/Distanzen konsistent behandeln"
  ),
  stringsAsFactors = FALSE
)

# Helper: Pakete pruefen -----------
check_pkgs <- function(pkgs, install_missing = FALSE) {
  stopifnot(is.character(pkgs))
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  
  if (length(missing_pkgs) == 0) {
    return(invisible(TRUE))
  }
  
  if (isTRUE(install_missing)) {
    utils::install.packages(missing_pkgs, dependencies = TRUE)
    still_missing <- missing_pkgs[!vapply(missing_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing) > 0) {
      stop("Folgende Pakete konnten nicht installiert/geladen werden: ", paste(still_missing, collapse = ", "))
    }
    return(invisible(TRUE))
  }
  
  stop(
    "Folgende Pakete fehlen: ", paste(missing_pkgs, collapse = ", "),
    ". Bitte installieren oder config$packages$install_missing <- TRUE setzen."
  )
}

check_pkgs(pkg_purpose$package, install_missing = isTRUE(config$packages$install_missing))
