# 00_00-Libraries

# Dieses Skript beinhaltet sämtliche zum Setup notwendigen Libraries


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


