# 00_01_03_Helper_Write
#tc steht hier für threshhold check und ist so angelegt, damit es nicht mit
# write.table o.ä. später verwechselt wird

# Pfade initialisieren --------------------------------------------------
tc_init_paths <- function(
    scope = "Explore",
    current_wd = config$paths$current_wd
) {
  stopifnot(is.character(scope), length(scope) == 1, nzchar(scope))
  
  base <- file.path(current_wd, "output", scope)
  
  paths <- list(
    scope       = scope,
    output_base = base,
    tables      = file.path(base, "tables"),
    figures     = file.path(base, "figures"),
    logfiles    = file.path(current_wd, "output", "logfiles")
  )
  
  for (p in paths[c("output_base", "tables", "figures", "logfiles")]) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  
  paths
}

# Logfile-Pfad (scope-basiert, mit Zeitstempel)---------------------------
# Zweck:
# - Erstellt einen Logfile-Pfad unter output/logfiles/
# - Dateiname enthält Datum (MMDD), Uhrzeit (HHMM) und Scope
#
# Beispiel:
#   output/logfiles/0130_1427_Explore.log
#
tc_log_path <- function(
    paths,
    time = Sys.time(),
    tz = "Europe/Berlin"
) {
  stopifnot(is.list(paths))
  stopifnot("logfiles" %in% names(paths))
  stopifnot("scope" %in% names(paths))
  stopifnot(is.character(paths$logfiles), length(paths$logfiles) == 1)
  stopifnot(is.character(paths$scope), length(paths$scope) == 1)
  
  # Zeitstempel zerlegen
  t <- as.POSIXlt(time, tz = tz)
  mmdd <- sprintf("%02d%02d", t$mon + 1, t$mday)   # MonatTag
  hhmm <- sprintf("%02d%02d", t$hour, t$min)       # StundeMinute
  
  # Scope dateisystem-sicher machen
  scope <- gsub("[^A-Za-z0-9._-]+", "_", paths$scope)
  
  # Sicherstellen, dass Log-Verzeichnis existiert
  dir.create(paths$logfiles, recursive = TRUE, showWarnings = FALSE)
  
  file.path(
    paths$logfiles,
    paste0(mmdd, "_", hhmm, "_", scope, ".log")
  )
}


# Grafiken speichern ------------------------------------------------------
tc_save_figure <- function(
    plot,
    fig_id,
    filename,
    paths,
    log_file,
    width = config$figures$width,
    height = config$figures$height,
    dpi = config$figures$dpi
) {
  stopifnot(is.character(fig_id), length(fig_id) == 1, nzchar(fig_id))
  stopifnot(is.character(filename), length(filename) == 1, nzchar(filename))
  stopifnot(is.list(paths), "figures" %in% names(paths))
  
  fig_dir <- paths$figures
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(fig_dir)) {
    stop("Figures directory does not exist and could not be created: ", fig_dir)
  }
  
  stem <- paste0(fig_id, "_", filename)
  png_path <- file.path(fig_dir, paste0(stem, ".png"))
  pdf_path <- file.path(fig_dir, paste0(stem, ".pdf"))
  
  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    create.dir = TRUE
  )
  
  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    create.dir = TRUE
  )
  
  log_write(log_file, "INFO", "Figure written: ", stem, " -> ", fig_dir)
  
  invisible(list(png = png_path, pdf = pdf_path))
}


# Table speichern (csv + rds + Excel-Collector) ------------------------
tc_save_table <- function(
    tab,
    table_id,
    paths,
    excel_list,
    log_file
) {
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(table_id), length(table_id) == 1, nzchar(table_id))
  
  csv_path <- file.path(paths$tables, paste0(table_id, ".csv"))
  rds_path <- file.path(paths$tables, paste0(table_id, ".rds"))
  
  readr::write_csv(tab, file = csv_path, na = "")
  saveRDS(tab, file = rds_path)
  
  excel_list[[table_id]] <- tab
  
  log_write(log_file, "INFO", "Table written: ", table_id,
            " (rows=", nrow(tab), ", cols=", ncol(tab), ")")
  
  excel_list
}


tc_write_excel <- function(
    excel_list,
    paths,
    filename = "Tables.xlsx",
    log_file = NULL
) {
  xlsx_path <- file.path(paths$tables, filename)
  writexl::write_xlsx(excel_list, path = xlsx_path)
  
  if (!is.null(log_file)) {
    log_write(log_file, "INFO", "Excel workbook written: ", xlsx_path)
  }
  
  xlsx_path
}


# tc_write_table ----------------------------------------------------------

tc_write_table <- function(
    tab,
    table_id,
    ctx,
    threshold = TRUE,
    threshold_n_col = "auto",
    threshold_label_cols = NULL,
    threshold_object_name = table_id,
    threshold_logname = "threshold_n_min"
) {
  stopifnot(is.list(ctx), all(c("paths", "log_file", "excel_tabs") %in% names(ctx)))
  
  ok <- TRUE
  
  if (isTRUE(threshold)) {
    ok <- passes_threshold_n_min(
      tab = tab,
      n_col = threshold_n_col,
      label_cols = threshold_label_cols,
      object_name = threshold_object_name,
      min_n = config$thresholds$n_min,
      mode = "any",
      logname = threshold_logname,
      base_dir = file.path(config$paths$current_wd, "output", "logfiles")
    )
  }
  
  if (isTRUE(ok)) {
    ctx$excel_tabs <- tc_save_table(
      tab = tab,
      table_id = table_id,
      paths = ctx$paths,
      excel_list = ctx$excel_tabs,
      log_file = ctx$log_file
    )
  } else {
    log_write(
      ctx$log_file,
      "WARN",
      "Output suppressed for table ", table_id, " (0 < n < n_min). ",
      "See threshold logfile in output/logfiles."
    )
  }
  
  ctx
}


# tc_write_figure ---------------------------------------------------------

tc_write_figure <- function(
    plot,
    fig_id,
    filename,
    ctx,
    threshold = TRUE,
    threshold_tab = NULL,
    threshold_n_col = "auto",
    threshold_label_cols = NULL,
    threshold_object_name = paste0(fig_id, "_", filename),
    threshold_logname = "threshold_n_min"
) {
  stopifnot(is.list(ctx), all(c("paths", "log_file") %in% names(ctx)))
  
  ok <- TRUE
  
  if (isTRUE(threshold)) {
    stopifnot(is.data.frame(threshold_tab))
    
    ok <- passes_threshold_n_min(
      tab = threshold_tab,
      n_col = threshold_n_col,
      label_cols = threshold_label_cols,
      object_name = threshold_object_name,
      min_n = config$thresholds$n_min,
      mode = "any",
      logname = threshold_logname,
      base_dir = file.path(config$paths$current_wd, "output", "logfiles")
    )
  }
  
  if (isTRUE(ok)) {
    tc_save_figure(
      plot = plot,
      fig_id = fig_id,
      filename = filename,
      paths = ctx$paths,
      log_file = ctx$log_file
    )
  } else {
    log_write(
      ctx$log_file,
      "WARN",
      "Output suppressed for figure ", fig_id, "_", filename, " (0 < n < n_min). ",
      "See threshold logfile in output/logfiles."
    )
  }
  
  ctx
}


