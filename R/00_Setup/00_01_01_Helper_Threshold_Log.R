# 00_01_01_Helper_Threshold_Log.R
# Zweck:
# - Zentrale Threshold-Logik zur Unterdrückung von Tabellen- und Grafik-Outputs
# - Ein Output wird unterdrückt, wenn für mindestens eine Gruppe gilt:
#       0 < n < min_n
# - In diesem Fall wird ein WARN-Logfile erzeugt
# - Wenn kein Verstoß vorliegt, wird kein Logfile geschrieben
#
# Einsatz:
# - Universell einsetzbar (Explore, Analyse, Reporting)
# - Für Tabellen UND für Grafiken (über deren zugrundeliegende Zähltabellen)


# Logfile-Pfad erzeugen ---------------------------------------------------
# output/logfiles/MMDD_logs/HHMM_<logname>.log

make_logfile_path <- function(
    logname,
    base_dir = file.path("output", "logfiles"),
    time = Sys.time(),
    tz = "Europe/Berlin"
) {
  stopifnot(is.character(logname), length(logname) == 1, nzchar(logname))
  
  # Logname bereinigen (dateisystem-sicher)
  safe_logname <- gsub("[^A-Za-z0-9._-]+", "_", logname)
  
  # Zeitstempel zerlegen
  t <- as.POSIXlt(time, tz = tz)
  mmdd <- sprintf("%02d%02d", t$mon + 1, t$mday)   # MonatTag
  hhmm <- sprintf("%02d%02d", t$hour, t$min)       # StundeMinute
  
  # Tagesordner anlegen
  day_dir <- file.path(base_dir, paste0(mmdd, "_logs"))
  if (!dir.exists(day_dir)) dir.create(day_dir, recursive = TRUE)
  
  file.path(day_dir, paste0(hhmm, "_", safe_logname, ".log"))
}


# Interne Hilfsfunktion ---------------------------------------------------
# sammelt Log-Zeilen für alle Zeilen mit 0 < n < min_n
# (schreibt noch nichts)

.collect_threshold_lines <- function(
    tab,
    n_col,
    label_cols,
    min_n,
    object_name
) {
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(n_col), length(n_col) == 1, n_col %in% names(tab))
  stopifnot(is.numeric(min_n), length(min_n) == 1, min_n > 0)
  
  n_vec <- tab[[n_col]]
  
  # Nur numerische n-Spalten sind sinnvoll prüfbar
  if (!is.numeric(n_vec) && !is.integer(n_vec)) return(character(0))
  
  # Kritische Gruppen: 0 < n < min_n
  bad_idx <- which(!is.na(n_vec) & n_vec > 0 & n_vec < min_n)
  if (length(bad_idx) == 0) return(character(0))
  
  # Falls keine Label-Spalten angegeben sind:
  # automatisch einige Kontextspalten verwenden
  if (is.null(label_cols)) {
    label_cols <- setdiff(names(tab), n_col)
    label_cols <- head(label_cols, 6)
  } else {
    label_cols <- intersect(label_cols, names(tab))
  }
  
  lines <- character(0)
  
  for (i in bad_idx) {
    labels <- if (length(label_cols) > 0) {
      paste(
        paste0(label_cols, "=", as.character(tab[i, label_cols, drop = TRUE])),
        collapse = "; "
      )
    } else {
      paste0("row=", i)
    }
    
    lines <- c(
      lines,
      sprintf(
        "object=%s | %s | %s=%s (min_n=%d)",
        object_name,
        labels,
        n_col,
        as.character(n_vec[i]),
        as.integer(min_n)
      )
    )
  }
  
  lines
}


# Öffentliche Kernfunktion ------------------------------------------------
# entscheidet, ob ein Output geschrieben werden darf
#
# Rückgabewert:
# - TRUE  -> Output darf erzeugt werden
# - FALSE -> Output MUSS unterdrückt werden (Threshold verletzt)
#
# Nebenwirkung:
# - Bei Verstoß wird ein WARN-Logfile erzeugt

passes_threshold_n_min <- function(
    tab,
    n_col = "n",
    label_cols = NULL,
    object_name = "object",
    min_n = config$thresholds$n_min,
    logname = "threshold_n_min",
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    tz = "Europe/Berlin",
    write_log = TRUE
) {
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(object_name), length(object_name) == 1, nzchar(object_name))
  stopifnot(is.numeric(min_n), length(min_n) == 1, min_n > 0)
  
  # Sammeln der kritischen Zeilen
  lines <- .collect_threshold_lines(
    tab = tab,
    n_col = n_col,
    label_cols = label_cols,
    min_n = min_n,
    object_name = object_name
  )
  
  # Kein Verstoß -> Output darf geschrieben werden
  if (length(lines) == 0) return(TRUE)
  
  # Verstoß -> optional Logfile schreiben
  if (isTRUE(write_log)) {
    
    logfile <- make_logfile_path(
      logname = logname,
      base_dir = base_dir,
      time = Sys.time(),
      tz = tz
    )
    
    ts <- format(Sys.time(), tz = tz, usetz = FALSE)
    
    header <- sprintf(
      "[%s] [WARN] Output unterdrückt (Kriterium: 0 < n < min_n). min_n=%d.",
      ts, as.integer(min_n)
    )
    
    out <- c(header, sprintf("[%s] [WARN] %s", ts, lines))
    
    writeLines(out, con = logfile, sep = "\n", useBytes = TRUE)
  }
  
  FALSE
}
