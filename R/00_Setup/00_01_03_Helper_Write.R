# 00_01_03_Helper_Write.R -----------------------------
## Zweck --------------------------
# Einheitliche Output-Logik mit Datenschutz-Threshold.
# Tabellen: CSV + RDS + Excel-Sammelmappe.
# Grafiken: PNG + PDF.
# Tabellen mit einzelnen kritischen Gruppen werden gruppenweise mit NA maskiert.
# Vollstaendig nicht pruefbare Outputs werden weiterhin unterdrueckt und in
# ctx$suppressed dokumentiert.

# Kohortenbezeichnungen -----------------------------
## Lesbare Bezeichnungen fuer Abbildungen und Tabellen --------------------------
tc_analysis_label <- function(
    output_group = config$analysis$active_output_group
) {
  stopifnot(
    is.character(output_group),
    length(output_group) == 1L,
    !is.na(output_group),
    nzchar(output_group)
  )

  if (identical(output_group, "Gesamt")) {
    return("Komplett")
  }

  if (identical(output_group, "Selektiv_Gesamt")) {
    return("Gesamt")
  }

  if (grepl("^[A-Z][0-9]{2}$", output_group)) {
    return(paste("ICD-10", output_group))
  }

  gsub("_", " ", output_group, fixed = TRUE)
}

tc_add_analysis_label_to_table <- function(tab, analysis_label) {
  stopifnot(is.data.frame(tab))

  if ("Auswertung" %in% names(tab)) {
    tab$Auswertung <- rep(analysis_label, nrow(tab))
    return(tab[, c("Auswertung", setdiff(names(tab), "Auswertung")), drop = FALSE])
  }

  data.frame(
    Auswertung = rep(analysis_label, nrow(tab)),
    tab,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

tc_add_analysis_label_to_plot <- function(plot, analysis_label) {
  current_title <- plot$labels$title
  current_title <- if (is.null(current_title) || length(current_title) == 0L) {
    ""
  } else {
    as.character(current_title)[1L]
  }

  title_with_label <- if (!nzchar(trimws(current_title))) {
    analysis_label
  } else if (
    identical(current_title, analysis_label) ||
      endsWith(current_title, paste0(" - ", analysis_label))
  ) {
    current_title
  } else {
    paste(current_title, analysis_label, sep = " - ")
  }

  plot + ggplot2::labs(title = title_with_label)
}

# Pfade initialisieren -----------------------------
tc_init_paths <- function(scope, current_wd = config$paths$current_wd) {
  stopifnot(is.character(scope), length(scope) == 1, nzchar(scope))
  base <- file.path(current_wd, "output", scope)
  paths <- list(
    scope = scope,
    output_base = base,
    tables = file.path(base, "tables"),
    figures = file.path(base, "figures"),
    rds = file.path(base, "rds"),
    logfiles = file.path(current_wd, "output", "logfiles")
  )
  path_names <- c("output_base", "tables", "figures", "rds", "logfiles")
  for (p in paths[path_names]) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  paths
}

# Logfile-Pfad -----------------------------
tc_log_path <- function(paths, time = Sys.time(), tz = "Europe/Berlin") {
  t <- as.POSIXlt(time, tz = tz)
  mmdd <- sprintf("%02d%02d", t$mon + 1, t$mday)
  hhmm <- sprintf("%02d%02d", t$hour, t$min)
  scope <- gsub("[^A-Za-z0-9._-]+", "_", paths$scope)
  file.path(paths$logfiles, paste0(mmdd, "_", hhmm, "_", scope, ".log"))
}

# Log schreiben -----------------------------
log_write <- function(log_file, level = "INFO", ...) {
  msg <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", level, "] ",
    paste0(..., collapse = "")
  )
  cat(msg, "\n", file = log_file, append = TRUE)
  message(msg)
  invisible(msg)
}

# Kontext initialisieren -----------------------------
tc_init_ctx <- function(scope) {
  paths <- tc_init_paths(scope = scope)
  log_file <- tc_log_path(paths)
  analysis_label <- tc_analysis_label()
  log_write(log_file, "INFO", "Started scope: ", scope, " | Auswertung: ", analysis_label)
  list(
    paths = paths,
    log_file = log_file,
    analysis_label = analysis_label,
    excel_tabs = list(),
    suppressed = data.frame(
      Auswertung = character(0),
      object_id = character(0),
      output_category = character(0),
      reason = character(0),
      stringsAsFactors = FALSE
    )
  )
}

# Unterdrueckung dokumentieren -----------------------------
tc_add_suppressed <- function(ctx, object_id, output_category, reason) {
  ctx$suppressed <- rbind(
    ctx$suppressed,
    data.frame(
      Auswertung = ctx$analysis_label,
      object_id = object_id,
      output_category = output_category,
      reason = reason,
      stringsAsFactors = FALSE
    )
  )
  log_write(ctx$log_file, "WARN", "Output suppressed: ", object_id, " | ", reason)
  ctx
}

# Tabelle speichern -----------------------------
tc_save_table <- function(tab, table_id, paths, excel_list, log_file) {
  stopifnot(is.data.frame(tab))
  csv_path <- file.path(paths$tables, paste0(table_id, ".csv"))
  rds_path <- file.path(paths$rds, paste0(table_id, ".rds"))
  readr::write_csv(tab, file = csv_path, na = "")
  saveRDS(tab, file = rds_path)
  excel_list[[table_id]] <- tab
  log_write(log_file, "INFO", "Table written: ", table_id, " (rows=", nrow(tab), ", cols=", ncol(tab), ")")
  excel_list
}

# Excel-Sammelmappe schreiben -----------------------------
tc_write_excel <- function(ctx, filename = paste0(ctx$paths$scope, "_Tables.xlsx")) {
  excel_list <- ctx$excel_tabs
  if (nrow(ctx$suppressed) > 0) {
    excel_list[["suppressed_outputs"]] <- ctx$suppressed
  }
  if (length(excel_list) == 0) {
    excel_list[["output_info"]] <- data.frame(
      Auswertung = ctx$analysis_label,
      info = "Keine Tabellen erzeugt oder alle Outputs unterdrueckt.",
      stringsAsFactors = FALSE
    )
  }
  xlsx_path <- file.path(ctx$paths$tables, filename)
  writexl::write_xlsx(excel_list, path = xlsx_path)
  log_write(ctx$log_file, "INFO", "Excel workbook written: ", xlsx_path)
  invisible(xlsx_path)
}

# Tabelle mit Threshold schreiben -----------------------------
tc_write_table <- function(
    tab,
    table_id,
    ctx,
    output_category = "aggregate_table",
    threshold = TRUE,
    threshold_n_col = NULL,
    threshold_label_cols = NULL,
    threshold_object_name = table_id
) {
  stopifnot(is.data.frame(tab))

  tab_out <- tab
  ok <- TRUE
  masked_rows <- integer(0)

  if (isTRUE(threshold)) {
    threshold_result <- mask_threshold_category(
      tab = tab,
      output_category = output_category,
      n_col = threshold_n_col,
      label_cols = threshold_label_cols,
      object_name = threshold_object_name
    )
    tab_out <- threshold_result$tab
    ok <- threshold_result$ok
    masked_rows <- threshold_result$masked_rows
  }

  if (isTRUE(ok)) {
    tab_out <- tc_add_analysis_label_to_table(tab_out, ctx$analysis_label)
    ctx$excel_tabs <- tc_save_table(tab_out, table_id, ctx$paths, ctx$excel_tabs, ctx$log_file)
    if (length(masked_rows) > 0) {
      log_write(
        ctx$log_file,
        "WARN",
        "Threshold rows masked with NA: ",
        table_id,
        " (rows=",
        paste(masked_rows, collapse = ","),
        ")"
      )
    }
  } else {
    ctx <- tc_add_suppressed(
      ctx = ctx,
      object_id = table_id,
      output_category = output_category,
      reason = "Datenschutz-Threshold nicht pruefbar: keine passende Fallzahlspalte gefunden."
    )
  }
  ctx
}

# Grafik speichern -----------------------------
tc_save_figure <- function(plot, fig_id, filename, paths, log_file, width = config$figures$width, height = config$figures$height, dpi = config$figures$dpi) {
  stem <- paste0(fig_id, "_", filename)
  png_path <- file.path(paths$figures, paste0(stem, ".png"))
  pdf_path <- file.path(paths$figures, paste0(stem, ".pdf"))
  ggplot2::ggsave(filename = png_path, plot = plot, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(filename = pdf_path, plot = plot, width = width, height = height)
  log_write(log_file, "INFO", "Figure written: ", stem)
  invisible(list(png = png_path, pdf = pdf_path))
}

# Grafik mit Threshold schreiben -----------------------------
## Kohortenbezeichnung wird vor dem Speichern an den Titel angehaengt --------------------------
# Hinweis: Der Plot muss bereits auf einer maskierten Datengrundlage erzeugt sein,
# wenn einzelne unsichere Gruppen als NA erscheinen sollen.
tc_write_figure <- function(
    plot,
    fig_id,
    filename,
    ctx,
    threshold_tab,
    output_category = "aggregate_figure",
    threshold = TRUE,
    threshold_n_col = NULL,
    threshold_label_cols = NULL,
    threshold_object_name = paste0(fig_id, "_", filename)
) {
  stopifnot(is.data.frame(threshold_tab))
  ok <- TRUE
  if (isTRUE(threshold)) {
    ok <- passes_threshold_category(
      tab = threshold_tab,
      output_category = output_category,
      n_col = threshold_n_col,
      label_cols = threshold_label_cols,
      object_name = threshold_object_name
    )
  }

  if (isTRUE(ok)) {
    plot <- tc_add_analysis_label_to_plot(plot, ctx$analysis_label)
    tc_save_figure(plot, fig_id, filename, ctx$paths, ctx$log_file)
  } else {
    ctx <- tc_add_suppressed(
      ctx = ctx,
      object_id = paste0(fig_id, "_", filename),
      output_category = output_category,
      reason = paste0("Datenschutz-Threshold verletzt: 0 < n < ", config$thresholds$n_min)
    )
  }
  ctx
}
