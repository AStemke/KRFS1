# Output-Dateinamen -----------------------------
## Kohortenspezifische Suffixe --------------------------
# Physische Dateien erhalten die aktive Analysegruppe als Suffix. Dadurch sind
# Tabellen und Grafiken auch ausserhalb ihrer Output-Ordner eindeutig zuordenbar.
# Excel-Sheetnamen bleiben unveraendert, damit sie kurz und stabil bleiben.

tc_output_suffix <- function(
    output_group = config$analysis$active_output_group
) {
  stopifnot(
    is.character(output_group),
    length(output_group) == 1L,
    !is.na(output_group),
    nzchar(output_group)
  )

  gsub("[^A-Za-z0-9._-]+", "_", output_group)
}

## Dateistamm erweitern --------------------------
tc_output_stem <- function(stem) {
  suffix <- tc_output_suffix()
  expected_suffix <- paste0("_", suffix)

  if (endsWith(stem, expected_suffix)) {
    return(stem)
  }

  paste0(stem, expected_suffix)
}

## Dateiname erweitern --------------------------
tc_output_filename <- function(filename) {
  extension <- tools::file_ext(filename)
  stem <- if (nzchar(extension)) {
    substr(filename, 1L, nchar(filename) - nchar(extension) - 1L)
  } else {
    filename
  }

  stem <- tc_output_stem(stem)

  if (nzchar(extension)) {
    paste0(stem, ".", extension)
  } else {
    stem
  }
}

# Tabellen -----------------------------
## CSV und RDS mit Kohortensuffix --------------------------
tc_save_table <- function(tab, table_id, paths, excel_list, log_file) {
  stopifnot(is.data.frame(tab))

  file_stem <- tc_output_stem(table_id)
  csv_path <- file.path(paths$tables, paste0(file_stem, ".csv"))
  rds_path <- file.path(paths$rds, paste0(file_stem, ".rds"))

  readr::write_csv(tab, file = csv_path, na = "")
  saveRDS(tab, file = rds_path)

  # Der Excel-Tab behaelt bewusst die kurze interne Tabellen-ID.
  excel_list[[table_id]] <- tab

  log_write(
    log_file,
    "INFO",
    "Table written: ",
    file_stem,
    " (rows=",
    nrow(tab),
    ", cols=",
    ncol(tab),
    ")"
  )

  excel_list
}

# Excel-Sammelmappe -----------------------------
## Dateiname mit Kohortensuffix --------------------------
tc_write_excel <- function(
    ctx,
    filename = paste0(ctx$paths$scope, "_Tables.xlsx")
) {
  excel_list <- ctx$excel_tabs

  if (nrow(ctx$suppressed) > 0L) {
    excel_list[["suppressed_outputs"]] <- ctx$suppressed
  }

  if (length(excel_list) == 0L) {
    excel_list[["output_info"]] <- data.frame(
      Auswertung = ctx$analysis_label,
      info = "Keine Tabellen erzeugt oder alle Outputs unterdrueckt.",
      stringsAsFactors = FALSE
    )
  }

  filename <- tc_output_filename(filename)
  xlsx_path <- file.path(ctx$paths$tables, filename)
  writexl::write_xlsx(excel_list, path = xlsx_path)
  log_write(ctx$log_file, "INFO", "Excel workbook written: ", xlsx_path)
  invisible(xlsx_path)
}

# Grafiken -----------------------------
## PNG und PDF mit Kohortensuffix --------------------------
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
  stem <- tc_output_stem(paste0(fig_id, "_", filename))
  png_path <- file.path(paths$figures, paste0(stem, ".png"))
  pdf_path <- file.path(paths$figures, paste0(stem, ".pdf"))

  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height
  )

  log_write(log_file, "INFO", "Figure written: ", stem)
  invisible(list(png = png_path, pdf = pdf_path))
}
