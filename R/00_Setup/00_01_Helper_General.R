# 00_01_Helper_General
# Hier werden allgemeine helper Funktionen definiert


# ------------------------------------------------------------------
# Source Setup-Helper (00_01_*.R) aus R/00_Setup
# - generisch über current_wd
# - Guard: genau 1x pro Session
# - Reihenfolge über Dateinamen
# ------------------------------------------------------------------

source_setup_helpers <- function(
    current_wd,
    setup_subdir = file.path("R", "00_Setup"),
    prefix = "00_01_",
    verbose = FALSE,
    local = FALSE,
    guard_name = ".setup_helpers_loaded"
) {
  # Guard
  if (exists(guard_name, envir = .GlobalEnv, inherits = FALSE)) {
    if (isTRUE(verbose)) {
      message("Setup-Helper bereits geladen (Guard: ", guard_name, ").")
    }
    return(invisible(NULL))
  }
  
  stopifnot(
    is.character(current_wd),
    length(current_wd) == 1,
    nzchar(current_wd),
    dir.exists(current_wd)
  )
  
  setup_dir <- file.path(current_wd, setup_subdir)
  stopifnot(dir.exists(setup_dir))
  
  files <- list.files(
    setup_dir,
    pattern = paste0("^", prefix, ".*\\.R$"),
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    warning("Keine Setup-Dateien mit Prefix ", prefix, " in ", setup_dir)
    return(invisible(character(0)))
  }
  
  # stabile Reihenfolge: 00_01_01_..., 00_01_02_...
  files <- sort(files)
  
  # Guard setzen (vor dem sourcen)
  assign(guard_name, TRUE, envir = .GlobalEnv)
  
  for (f in files) {
    if (isTRUE(verbose)) message("Sourcing: ", basename(f))
    source(f, local = local)
  }
  
  invisible(files)
}


# Logfiles for Threshold --------------------------------------------------

stopifnot(exists("current_wd"))

source_setup_helpers(
  current_wd = current_wd,
  verbose = TRUE
)
