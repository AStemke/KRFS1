# 00_01-Helper_Setup.R -----------
# Zweck:
# - Source-Helper fuer alle Unterdateien mit Prefix, z.B. 00_01_*.R
# - Reihenfolge ueber Dateinamen steuerbar

source_setup_helpers <- function(
    current_wd = config$paths$current_wd,
    setup_subdir = file.path("R", "00_Setup"),
    prefix = "00_01_",
    verbose = FALSE,
    local = FALSE
) {
  stopifnot(is.character(current_wd), length(current_wd) == 1, nzchar(current_wd))
  setup_dir <- file.path(current_wd, setup_subdir)
  if (!dir.exists(setup_dir)) {
    warning("Skriptordner existiert nicht: ", setup_dir)
    return(invisible(character(0)))
  }
  
  files <- list.files(
    setup_dir,
    pattern = paste0("^", prefix, ".*\\.R$"),
    full.names = TRUE
  )
  files <- sort(files)
  
  if (length(files) == 0) {
    if (isTRUE(verbose)) message("Keine Skripte gefunden fuer Prefix: ", prefix)
    return(invisible(character(0)))
  }
  
  for (f in files) {
    if (isTRUE(verbose)) message("Sourcing: ", basename(f))
    source(f, local = local)
  }
  
  invisible(files)
}

# Alle Setup-Helfer laden -----------
source_setup_helpers(
  current_wd = config$paths$current_wd,
  setup_subdir = file.path("R", "00_Setup"),
  prefix = "00_01_",
  verbose = FALSE,
  local = FALSE
)
