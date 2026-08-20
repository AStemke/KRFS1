# 00_02_01_Data_Load.R -----------
# Zweck:
# - Rohdaten gemaess README aus data/Dummy/raw oder data/Productive/raw einlesen
# - Optional vorhandenes ADS aus data/*/ads nutzen
# - Dummy_Datensatz_DATUM.xlsx und Legende erkennen

.find_single_file <- function(dir, pattern, label) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("Keine Datei gefunden fuer ", label, " in ", dir, " mit pattern ", pattern)
  }
  if (length(files) > 1) {
    stop("Mehr als eine Datei gefunden fuer ", label, ": ", paste(basename(files), collapse = ", "))
  }
  files[[1]]
}

.extract_date_from_filename <- function(path) {
  m <- regmatches(basename(path), regexpr("[0-9]{8}", basename(path)))
  if (length(m) == 0 || m == "") return(format(Sys.Date(), "%Y%m%d"))
  m
}

load_ads <- function(current_wd = config$paths$current_wd, productive = config$data$productive) {
  data_kind <- if (isTRUE(productive)) "Productive" else "Dummy"
  raw_dir <- file.path(current_wd, "data", data_kind, "raw")
  ads_dir <- file.path(current_wd, "data", data_kind, "ads")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ads_dir, recursive = TRUE, showWarnings = FALSE)
  
  ads_files <- list.files(ads_dir, pattern = "\\.rds$", full.names = TRUE)
  if (isTRUE(config$data$prefer_existing_ads) && length(ads_files) > 0) {
    ads_file <- ads_files[order(file.info(ads_files)$mtime, decreasing = TRUE)][[1]]
    ads <- readRDS(ads_file)
    return(list(data = ads$data, legend = ads$legend, meta = ads$meta))
  }
  
  data_pattern <- if (isTRUE(productive)) config$data$productive_data_regex else config$data$dummy_data_regex
  legend_pattern <- if (isTRUE(productive)) config$data$productive_legend_regex else config$data$dummy_legend_regex
  
  data_file <- .find_single_file(raw_dir, data_pattern, paste0(data_kind, " Datensatz"))
  legend_file <- .find_single_file(raw_dir, legend_pattern, paste0(data_kind, " Legende"))
  file_date <- .extract_date_from_filename(data_file)
  
  #Productive ist bereites RDS, für Dummy oder Test
  if(config$data$productive == FALSE){
    dat <- readxl::read_excel(data_file, sheet = config$data$sheet_data, .name_repair = "minimal")
  } else{
    dat <-read_rds(data_file)
  }
  
  legend <- readxl::read_excel(legend_file, sheet = config$data$sheet_legend, .name_repair = "minimal")
  dat <- as.data.frame(dat)
  legend <- as.data.frame(legend)
  names(dat) <- trimws(names(dat))
  names(legend) <- trimws(names(legend))
  
  meta <- list(
    data_kind = data_kind,
    source_data_file = data_file,
    source_legend_file = legend_file,
    source_date = file_date,
    created_at = Sys.time()
  )
  
  ads <- list(data = dat, legend = legend, meta = meta)
  saveRDS(ads, file = file.path(ads_dir, paste0("ads_", file_date, ".rds")))
  ads
}
