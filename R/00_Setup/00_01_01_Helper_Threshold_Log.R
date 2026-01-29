# 00_01_01_Helper_Threshold_Log

# Kontrolliert, dass wenigstens 3 Beobachtungen in der jeweiligen Tabelle vorliegen
# Erstellt dazu passende Logfiles

# output/logfiles/MMDD_logs/HHMM_logname.log
make_logfile_path <- function(
    logname,
    base_dir = file.path("output", "logfiles"),
    time = Sys.time(),
    tz = "Europe/Berlin"
) {
  safe_logname <- gsub("[^A-Za-z0-9._-]+", "_", logname)
  t <- as.POSIXlt(time, tz = tz)
  
  mmdd <- sprintf("%02d%02d", t$mon + 1, t$mday)   # MonatTag
  hhmm <- sprintf("%02d%02d", t$hour, t$min)       # StundeMinute
  
  day_dir <- file.path(base_dir, paste0(mmdd, "_logs"))
  if (!dir.exists(day_dir)) dir.create(day_dir, recursive = TRUE)
  
  file.path(day_dir, paste0(hhmm, "_", safe_logname, ".log"))
}

# Prüft eine Tabelle und liefert Log-Zeilen zurück (ohne zu schreiben)
.collect_min_n_lines <- function(tab, n_col, label_cols, min_n, table_name) {
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(n_col), length(n_col) == 1, n_col %in% names(tab))
  
  bad_idx <- which(is.na(tab[[n_col]]) | tab[[n_col]] < min_n)
  if (length(bad_idx) == 0) return(character(0))
  
  if (is.null(label_cols)) {
    label_cols <- setdiff(names(tab), n_col)
    label_cols <- head(label_cols, 6)
  } else {
    label_cols <- intersect(label_cols, names(tab))
  }
  
  lines <- character(0)
  for (i in bad_idx) {
    labels <- if (length(label_cols) > 0) {
      paste(paste0(label_cols, "=", as.character(tab[i, label_cols, drop = TRUE])), collapse = "; ")
    } else {
      paste0("row=", i)
    }
    
    lines <- c(
      lines,
      sprintf("table=%s | %s | %s=%s",
              table_name, labels, n_col, as.character(tab[[n_col]][i]))
    )
  }
  lines
}

# Mehrere Tabellen prüfen; NUR wenn irgendwo n < 3 ist wird EIN Logfile geschrieben.
#
# tabs:
#   - named list: list(stage_tbl = ..., overview_tbl = ...)
#   - oder unnamed list: names werden dann "table_1", "table_2", ...
#
# n_col / label_cols:
#   - entweder single value (gilt für alle)
#   - oder named list pro Tabelle (names müssen zu tabs passen)
#
log_min_n_many <- function(
    tabs,
    n_col = "n",
    label_cols = NULL,
    logname = "threshold_min_n3",
    min_n = threshhold_n, #TODO: Hier bitte Mindestanzahl bestätigen
    tz = "Europe/Berlin"
) {
  stopifnot(is.list(tabs), length(tabs) > 0)
  
  tab_names <- names(tabs)
  if (is.null(tab_names) || any(!nzchar(tab_names))) {
    tab_names <- paste0("table_", seq_along(tabs))
    names(tabs) <- tab_names
  }
  
  # helper: hole pro Tabelle den passenden n_col / label_cols
  get_per_table <- function(x, nm, default) {
    if (is.list(x)) {
      if (!is.null(names(x)) && nm %in% names(x)) return(x[[nm]])
      return(default)
    }
    x
  }
  
  # Sammeln, aber noch nichts schreiben
  all_lines <- character(0)
  per_table_counts <- integer(0)
  
  for (nm in tab_names) {
    tab <- tabs[[nm]]
    this_n_col <- get_per_table(n_col, nm, default = "n")
    this_label_cols <- get_per_table(label_cols, nm, default = NULL)
    
    lines <- .collect_min_n_lines(
      tab = tab,
      n_col = this_n_col,
      label_cols = this_label_cols,
      min_n = min_n,
      table_name = nm
    )
    
    per_table_counts[nm] <- length(lines)
    if (length(lines) > 0) all_lines <- c(all_lines, lines)
  }
  
  # Alles ok -> kein Logfile
  if (length(all_lines) == 0) return(invisible(NULL))
  
  # Jetzt erst Logfile erzeugen + schreiben
  logfile <- make_logfile_path(logname = logname, tz = tz)
  ts <- format(Sys.time(), tz = tz, usetz = FALSE)
  
  summary_bits <- paste0(names(per_table_counts), "=", per_table_counts, collapse = ", ")
  header <- sprintf("[%s] [WARN] Mindest-n verletzt (min_n=%d). Treffer pro Tabelle: %s",
                    ts, min_n, summary_bits)
  
  out <- c(header, sprintf("[%s] [WARN] %s", ts, all_lines))
  writeLines(out, con = logfile, sep = "\n", useBytes = TRUE)
  
  invisible(logfile)
}
