# 00_01_01_Helper_Threshold_Log.R
# Zweck:
# - Datenschutz-Threshold für Outputs (Tabellen/Grafiken)
# - Unterdrückung, wenn irgendwo gilt: 0 < n < min_n
# - mode="any": Unterdrückung sobald irgendeine relevante Zählspalte irgendwo kritisch ist
# - Automatische Erkennung relevanter Zählspalten über n_col = "auto"
# - Universell einsetzbar für tabellen und tabellen für Grafiken

# Logfile-Pfad erzeugen ---------------------------------------------------
# output/logfiles/MMDD_logs/HHMM_<logname>.log

make_logfile_path <- function(
    logname,
    base_dir = file.path("output", "logfiles"),
    time = Sys.time(),
    tz = "Europe/Berlin"
) {
  stopifnot(is.character(logname), length(logname) == 1, nzchar(logname))
  
  safe_logname <- gsub("[^A-Za-z0-9._-]+", "_", logname)
  t <- as.POSIXlt(time, tz = tz)
  
  mmdd <- sprintf("%02d%02d", t$mon + 1, t$mday)
  hhmm <- sprintf("%02d%02d", t$hour, t$min)
  
  day_dir <- file.path(base_dir, paste0(mmdd, "_logs"))
  if (!dir.exists(day_dir)) dir.create(day_dir, recursive = TRUE)
  
  file.path(day_dir, paste0(hhmm, "_", safe_logname, ".log"))
}


# Interne Hilfsfunktion: automatische Erkennung von Zählspalten -----------
# Strategie (konservativ, datenschutzfreundlich):
# - numerisch UND ganzzahlig (keine Nachkommastellen)
# - optional zusätzlich Name-Matching per Regex (z.B. ^n)


.detect_n_cols_auto <- function(
    tab,
    regex = "^(n|N)",
    require_integerish = TRUE,
    exclude = character(0)
) {
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(regex), length(regex) == 1)
  stopifnot(is.character(exclude))
  
  cols <- names(tab)
  
  is_num <- vapply(tab, function(x) is.numeric(x) || is.integer(x), logical(1))
  cols <- cols[is_num]
  
  if (require_integerish) {
    integerish <- vapply(tab[cols], function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(TRUE)
      all(x %% 1 == 0)
    }, logical(1))
    cols <- cols[integerish]
  }
  
  if (nzchar(regex)) {
    cols <- cols[grepl(regex, cols)]
  }
  
  cols <- setdiff(cols, exclude)
  cols
}


# Kernfunktion: TRUE/FALSE, ob Output geschrieben werden darf -------------
# Kriterium pro Zeile und relevanter n-Spalte:
#   kritisch, wenn: 0 < n < min_n
#
# n_col:
#   - "auto" -> automatische Erkennung (siehe .detect_n_cols_auto)
#   - character vector -> explizite Spalten
#
# mode:
#   - "any" (Default) -> unterdrücken, sobald irgendeine relevante n-Spalte irgendwo kritisch ist
#   - "all"           -> unterdrücken nur, wenn jede relevante n-Spalte irgendwo kritisch ist

passes_threshold_n_min <- function(
    tab,
    n_col = "auto",
    label_cols = NULL,
    object_name = "object",
    min_n = config$thresholds$n_min,
    mode = c("any", "all"),
    logname = "threshold_n_min",
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    tz = "Europe/Berlin",
    write_log = TRUE,
    # auto-detect settings (werden bei n_col="auto" verwendet)
    auto_regex = config$thresholds$auto_regex,
    auto_require_integerish = config$thresholds$auto_require_integerish,
    auto_exclude = config$thresholds$auto_exclude
) {
  mode <- match.arg(mode)
  
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(object_name), length(object_name) == 1, nzchar(object_name))
  stopifnot(is.numeric(min_n), length(min_n) == 1, !is.na(min_n), min_n > 0)
  
  # relevante n-Spalten bestimmen
  if (identical(n_col, "auto")) {
    n_cols <- .detect_n_cols_auto(
      tab = tab,
      regex = if (!is.null(auto_regex)) auto_regex else "^(n|N)",
      require_integerish = isTRUE(auto_require_integerish),
      exclude = if (!is.null(auto_exclude)) auto_exclude else character(0)
    )
  } else if (is.character(n_col) && length(n_col) >= 1) {
    n_cols <- intersect(n_col, names(tab))
  } else {
    stop("`n_col` muss entweder \"auto\" oder ein character-Vektor mit Spaltennamen sein.")
  }
  
  # keine prüfbaren n-Spalten -> keine Unterdrückung
  if (length(n_cols) == 0) return(TRUE)
  
  # Label-Spalten festlegen (für Logtext)
  if (is.null(label_cols)) {
    label_cols <- setdiff(names(tab), n_cols)
    label_cols <- head(label_cols, 6)
  } else {
    label_cols <- intersect(label_cols, names(tab))
  }
  
  # pro n-Spalte prüfen, ob irgendwo 0<n<min_n vorkommt
  violated_by_col <- stats::setNames(logical(length(n_cols)), n_cols)
  lines_all <- character(0)
  
  for (col in n_cols) {
    n_vec <- tab[[col]]
    if (!is.numeric(n_vec) && !is.integer(n_vec)) {
      violated_by_col[[col]] <- FALSE
      next
    }
    
    bad_idx <- which(!is.na(n_vec) & n_vec > 0 & n_vec < min_n)
    violated_by_col[[col]] <- length(bad_idx) > 0
    
    if (length(bad_idx) > 0) {
      for (i in bad_idx) {
        labels <- if (length(label_cols) > 0) {
          paste(
            paste0(label_cols, "=", as.character(tab[i, label_cols, drop = TRUE])),
            collapse = "; "
          )
        } else {
          paste0("row=", i)
        }
        
        lines_all <- c(
          lines_all,
          sprintf("object=%s | %s | %s=%s (min_n=%d)",
                  object_name, labels, col, as.character(n_vec[i]), as.integer(min_n))
        )
      }
    }
  }
  
  violated <- if (mode == "any") any(violated_by_col) else all(violated_by_col)
  
  if (!violated) return(TRUE)
  
  if (isTRUE(write_log)) {
    logfile <- make_logfile_path(
      logname = logname,
      base_dir = base_dir,
      time = Sys.time(),
      tz = tz
    )
    
    ts <- format(Sys.time(), tz = tz, usetz = FALSE)
    summary_bits <- paste0(names(violated_by_col), "=", as.integer(violated_by_col), collapse = ", ")
    
    header <- sprintf(
      "[%s] [WARN] Output unterdrückt (Kriterium: 0 < n < min_n). min_n=%d, mode=%s. n_cols=[%s]. Violations: %s",
      ts, as.integer(min_n), mode, paste(n_cols, collapse = ", "), summary_bits
    )
    
    out <- c(header, sprintf("[%s] [WARN] %s", ts, lines_all))
    writeLines(out, con = logfile, sep = "\n", useBytes = TRUE)
  }
  
  FALSE
}



# Anwendungsbeispiel ------------------------------------------------------


## Tabellen --------------------------------------------------------------
# if (passes_threshold_n_min(
#   tab = age_group_tab,
#   n_col = "n",
#   label_cols = c("age_group"),
#   object_name = "01_06_age_groups"
# )) {
#   explore_write_table(...)
# }


## Grafiken --------------------------------------------------------------
# if (passes_threshold_n_min(
#   tab = plot_data,
#   n_col = "n",
#   label_cols = c("age_group"),
#   object_name = "01_06_age_groups_bar"
# )) {
#   explore_write_figure(...)
# }



## Mehrere Spalten -------------------------------------------------------
# ok <- passes_threshold_n_min(
#   tab = some_tab,
#   n_col = c("n_total", "n_events"),
#   label_cols = c("age_group", "sex"),
#   object_name = "02_03_some_output",
#   mode = "any"   # sobald irgendwo 1 oder 2 auftauchen -> unterdrücken
# )


## Automatisch alle, nicht explizit angegeben ----------------------------
# ok <- passes_threshold_n_min(
#   tab = age_group_tab,
#   n_col = "auto",                 # oder explizit "n"
#   label_cols = c("age_group"),
#   object_name = "01_06_age_groups",
#   mode = "any",                   # Datenschutz
#   logname = "Explore_threshold"
# )
# 
# if (ok) {
#   # write table / figure
# }






