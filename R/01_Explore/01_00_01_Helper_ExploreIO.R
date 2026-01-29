# 01_00_01_Helper_ExploreIO.R
# Zweck:
# - Einheitliche IO-Logik für Explore
# - Tabellen: csv + rds + Excel-Sammelmappe
# - Grafiken: eigener Unterordner je Explore-ID
# - Logs: output/logfiles (MMDD)

# ------------------------------------------------------------
# Pfade initialisieren
# ------------------------------------------------------------
explore_init_paths <- function(current_wd) {
  
  paths <- list(
    explore_base = file.path(current_wd, "output", "Explore"),
    tables       = file.path(current_wd, "output", "Explore", "tables"),
    figures      = file.path(current_wd, "output", "Explore", "figures"),
    logfiles     = file.path(current_wd, "output", "logfiles")
  )
  
  for (p in paths) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  
  paths
}

# ------------------------------------------------------------
# Logfile
# ------------------------------------------------------------
explore_log_path <- function(paths, date = Sys.Date()) {
  stamp <- format(date, "%m%d")  # MMDD
  file.path(paths$logfiles, paste0(stamp, "_explore.log"))
}

log_write <- function(log_file, level = "INFO", ...) {
  
  msg <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", level, "] ",
    paste0(..., collapse = "")
  )
  
  cat(msg, "\n", file = log_file, append = TRUE)
  message(msg)
}

# ------------------------------------------------------------
# Tabellen: csv + rds + Excel-Collector
# ------------------------------------------------------------
explore_write_table <- function(tab,
                                table_id,
                                paths,
                                excel_list,
                                log_file) {
  
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(table_id), length(table_id) == 1)
  
  csv_path <- file.path(paths$tables, paste0(table_id, ".csv"))
  rds_path <- file.path(paths$tables, paste0(table_id, ".rds"))
  
  readr::write_csv(tab, file = csv_path, na = "")
  saveRDS(tab, file = rds_path)
  
  excel_list[[table_id]] <- tab
  
  log_write(
    log_file,
    "INFO",
    "Table written: ", table_id,
    " (rows=", nrow(tab), ", cols=", ncol(tab), ")"
  )
  
  excel_list
}

# ------------------------------------------------------------
# Excel mit allen Explore-Tabellen
# ------------------------------------------------------------
explore_write_excel <- function(excel_list,
                                paths,
                                filename = "Explore_Tables.xlsx",
                                log_file = NULL) {
  
  xlsx_path <- file.path(paths$tables, filename)
  writexl::write_xlsx(excel_list, path = xlsx_path)
  
  if (!is.null(log_file)) {
    log_write(log_file, "INFO", "Excel workbook written: ", xlsx_path)
  }
  
  xlsx_path
}

# ------------------------------------------------------------
# Grafiken
# ------------------------------------------------------------
explore_write_figure <- function(plot,
                                 fig_id,
                                 filename,
                                 paths,
                                 log_file,
                                 width = fig_width,
                                 height = fig_height,
                                 dpi = fig_id) {
  
  stopifnot(is.character(fig_id), length(fig_id) == 1)
  
  fig_dir <- file.path(paths$figures, fig_id)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  
  png_path <- file.path(fig_dir, paste0(filename, ".png"))
  pdf_path <- file.path(fig_dir, paste0(filename, ".pdf"))
  
  ggplot2::ggsave(png_path, plot = plot,
                  width = width, height = height, dpi = dpi)
  ggplot2::ggsave(pdf_path, plot = plot,
                  width = width, height = height)
  
  log_write(
    log_file,
    "INFO",
    "Figure written: ", fig_id, "/", filename
  )
  
  invisible(list(png = png_path, pdf = pdf_path))
}
