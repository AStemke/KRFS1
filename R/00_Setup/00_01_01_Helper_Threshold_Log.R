# 00_01_01_Helper_Threshold_Log.R -----------------------------
## Zweck --------------------------
# Datenschutz-Threshold fuer Tabellen, Grafiken, Karten, Tests und Modelle.
# Regel: Jede ausgegebene Zellfallzahl muss entweder 0 oder >= min_n sein.
# Kritisch ist: 0 < n < min_n.
# Tabellen koennen kritische Gruppen zeilenweise mit NA maskieren.
# Konservatives Verhalten: Wenn eine pruefpflichtige Ausgabe keine Fallzahlspalte
# besitzt, wird sie unterdrueckt und geloggt.

# Verzeichnisse -----------------------------
## Logverzeichnis robust anlegen --------------------------
# Auf Netzlaufwerken kann dir.create(..., recursive = TRUE) versuchen, zu weit
# oben im Pfad neue Ordner anzulegen. Deshalb werden nur die tatsaechlich
# fehlenden Unterordner unterhalb des naechsten existierenden Elternordners
# schrittweise erzeugt.
.ensure_log_directory <- function(path) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    !is.na(path),
    nzchar(path)
  )

  if (dir.exists(path)) {
    return(invisible(path))
  }

  path <- gsub("\\\\", "/", path)
  missing_paths <- character(0)
  current_path <- path

  while (!dir.exists(current_path)) {
    parent_path <- dirname(current_path)

    if (identical(parent_path, current_path)) {
      stop(
        paste0(
          "Kein existierender Elternordner fuer Log-Verzeichnis gefunden:\n",
          path
        ),
        call. = FALSE
      )
    }

    missing_paths <- c(current_path, missing_paths)
    current_path <- parent_path
  }

  for (dir_path in missing_paths) {
    if (!dir.exists(dir_path)) {
      created <- dir.create(
        dir_path,
        recursive = FALSE,
        showWarnings = FALSE
      )

      if (!isTRUE(created) && !dir.exists(dir_path)) {
        stop(
          paste0(
            "Log-Verzeichnis konnte nicht angelegt werden:\n",
            dir_path,
            "\n\nExistierender Elternordner:\n",
            dirname(dir_path),
            "\n\nBitte Schreibberechtigung und Erreichbarkeit des Netzlaufwerks pruefen."
          ),
          call. = FALSE
        )
      }
    }
  }

  invisible(path)
}

# Logfile-Pfad -----------------------------
## Pfad mit Tagesordner erzeugen --------------------------
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

  .ensure_log_directory(day_dir)

  file.path(day_dir, paste0(hhmm, "_", safe_logname, ".log"))
}

# Threshold-Log schreiben -----------------------------
## Logdatei --------------------------
threshold_log_write <- function(
    lines,
    logname = "threshold_n_min",
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    tz = "Europe/Berlin"
) {
  logfile <- make_logfile_path(
    logname = logname,
    base_dir = base_dir,
    time = Sys.time(),
    tz = tz
  )
  ts <- format(Sys.time(), tz = tz, usetz = FALSE)
  writeLines(
    paste0("[", ts, "] ", lines),
    con = logfile,
    sep = "\n",
    useBytes = TRUE
  )
  invisible(logfile)
}

# Fallzahlspalten -----------------------------
## Automatische Erkennung --------------------------
.detect_n_cols_auto <- function(
    tab,
    regex = config$thresholds$auto_regex,
    require_integerish = config$thresholds$auto_require_integerish,
    exclude = config$thresholds$auto_exclude
) {
  stopifnot(is.data.frame(tab))
  cols <- names(tab)
  is_num <- vapply(tab, function(x) is.numeric(x) || is.integer(x), logical(1))
  cols <- cols[is_num]

  if (isTRUE(require_integerish) && length(cols) > 0) {
    integerish <- vapply(tab[cols], function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(TRUE)
      all(abs(x - round(x)) < .Machine$double.eps^0.5)
    }, logical(1))
    cols <- cols[integerish]
  }

  if (!is.null(regex) && nzchar(regex)) {
    cols <- cols[grepl(regex, cols)]
  }
  setdiff(cols, exclude)
}

## Fallzahlspalten aufloesen --------------------------
.resolve_n_cols <- function(tab, n_col = "auto") {
  if (identical(n_col, "auto")) {
    return(.detect_n_cols_auto(tab))
  }
  if (is.character(n_col)) {
    return(intersect(n_col, names(tab)))
  }
  stop("`n_col` muss entweder 'auto' oder ein character-Vektor sein.")
}

# Threshold-Pruefung -----------------------------
## Zell-Threshold pruefen --------------------------
passes_threshold_n_min <- function(
    tab,
    n_col = "auto",
    label_cols = NULL,
    object_name = "object",
    min_n = config$thresholds$n_min,
    mode = c("any", "all"),
    require_count_cols = FALSE,
    logname = "threshold_n_min",
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    tz = "Europe/Berlin",
    write_log = TRUE
) {
  mode <- match.arg(mode)
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(object_name), length(object_name) == 1, nzchar(object_name))
  stopifnot(is.numeric(min_n), length(min_n) == 1, !is.na(min_n), min_n > 0)

  n_cols <- .resolve_n_cols(tab, n_col = n_col)

  if (length(n_cols) == 0) {
    if (isTRUE(require_count_cols)) {
      if (isTRUE(write_log)) {
        threshold_log_write(
          lines = paste0(
            "[WARN] Output unterdrueckt: keine pruefbare Fallzahlspalte gefunden. object=",
            object_name, ", n_col=", paste(n_col, collapse = ",")
          ),
          logname = logname,
          base_dir = base_dir,
          tz = tz
        )
      }
      return(FALSE)
    }
    return(TRUE)
  }

  if (is.null(label_cols)) {
    label_cols <- head(setdiff(names(tab), n_cols), 6)
  } else {
    label_cols <- intersect(label_cols, names(tab))
  }

  violated_by_col <- stats::setNames(logical(length(n_cols)), n_cols)
  lines_all <- character(0)

  for (col in n_cols) {
    n_vec <- tab[[col]]
    bad_idx <- which(!is.na(n_vec) & n_vec > 0 & n_vec < min_n)
    violated_by_col[[col]] <- length(bad_idx) > 0

    if (length(bad_idx) > 0) {
      for (i in bad_idx) {
        labels <- if (length(label_cols) > 0) {
          paste(
            paste0(
              label_cols,
              "=",
              as.character(tab[i, label_cols, drop = TRUE])
            ),
            collapse = "; "
          )
        } else {
          paste0("row=", i)
        }
        lines_all <- c(
          lines_all,
          sprintf(
            "[WARN] object=%s | %s | %s=%s (min_n=%d)",
            object_name,
            labels,
            col,
            as.character(n_vec[i]),
            as.integer(min_n)
          )
        )
      }
    }
  }

  violated <- if (mode == "any") any(violated_by_col) else all(violated_by_col)
  if (!violated) return(TRUE)

  if (isTRUE(write_log)) {
    header <- sprintf(
      "[WARN] Output unterdrueckt (Kriterium: 0 < n < min_n). object=%s, min_n=%d, mode=%s, n_cols=[%s]",
      object_name,
      as.integer(min_n),
      mode,
      paste(n_cols, collapse = ", ")
    )
    threshold_log_write(
      lines = c(header, lines_all),
      logname = logname,
      base_dir = base_dir,
      tz = tz
    )
  }

  FALSE
}

# Threshold-Maskierung -----------------------------
## Kritische Gruppen zeilenweise mit NA maskieren --------------------------
mask_threshold_n_min <- function(
    tab,
    n_col = "auto",
    label_cols = NULL,
    object_name = "object",
    min_n = config$thresholds$n_min,
    mode = c("any", "all"),
    require_count_cols = FALSE,
    logname = "threshold_n_min",
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    tz = "Europe/Berlin",
    write_log = TRUE
) {
  mode <- match.arg(mode)
  stopifnot(is.data.frame(tab))
  stopifnot(is.character(object_name), length(object_name) == 1, nzchar(object_name))
  stopifnot(is.numeric(min_n), length(min_n) == 1, !is.na(min_n), min_n > 0)

  n_cols <- .resolve_n_cols(tab, n_col = n_col)

  if (length(n_cols) == 0) {
    if (isTRUE(require_count_cols) && isTRUE(write_log)) {
      threshold_log_write(
        lines = paste0(
          "[WARN] Output nicht maskierbar: keine pruefbare Fallzahlspalte gefunden. object=",
          object_name, ", n_col=", paste(n_col, collapse = ",")
        ),
        logname = logname,
        base_dir = base_dir,
        tz = tz
      )
    }
    return(list(
      tab = tab,
      ok = !isTRUE(require_count_cols),
      masked_rows = integer(0),
      n_cols = character(0),
      label_cols = character(0)
    ))
  }

  if (is.null(label_cols)) {
    label_cols <- head(setdiff(names(tab), n_cols), 6)
  } else {
    label_cols <- intersect(label_cols, names(tab))
  }

  bad_matrix <- matrix(
    FALSE,
    nrow = nrow(tab),
    ncol = length(n_cols),
    dimnames = list(NULL, n_cols)
  )

  for (j in seq_along(n_cols)) {
    n_vec <- tab[[n_cols[[j]]]]
    bad_matrix[, j] <- !is.na(n_vec) & n_vec > 0 & n_vec < min_n
  }

  bad_rows <- if (mode == "any") {
    rowSums(bad_matrix) > 0
  } else {
    rowSums(bad_matrix) == length(n_cols)
  }
  bad_idx <- which(bad_rows)

  if (length(bad_idx) == 0) {
    return(list(
      tab = tab,
      ok = TRUE,
      masked_rows = integer(0),
      n_cols = n_cols,
      label_cols = label_cols
    ))
  }

  masked_tab <- tab
  mask_cols <- setdiff(names(masked_tab), label_cols)
  for (col in mask_cols) {
    masked_tab[[col]][bad_idx] <- NA
  }

  if (isTRUE(write_log)) {
    lines_all <- vapply(bad_idx, function(i) {
      labels <- if (length(label_cols) > 0) {
        paste(
          paste0(
            label_cols,
            "=",
            as.character(tab[i, label_cols, drop = TRUE])
          ),
          collapse = "; "
        )
      } else {
        paste0("row=", i)
      }
      bad_cols_i <- n_cols[bad_matrix[i, ]]
      bad_values <- paste(
        paste0(
          bad_cols_i,
          "=",
          vapply(
            bad_cols_i,
            function(col) as.character(tab[[col]][i]),
            character(1)
          )
        ),
        collapse = "; "
      )
      sprintf(
        "[WARN] object=%s | %s | maskiert: %s (min_n=%d)",
        object_name,
        labels,
        bad_values,
        as.integer(min_n)
      )
    }, character(1))

    header <- sprintf(
      "[WARN] Gruppen mit NA maskiert (Kriterium: 0 < n < min_n). object=%s, min_n=%d, mode=%s, n_cols=[%s], rows=%d",
      object_name,
      as.integer(min_n),
      mode,
      paste(n_cols, collapse = ", "),
      length(bad_idx)
    )
    threshold_log_write(
      lines = c(header, lines_all),
      logname = logname,
      base_dir = base_dir,
      tz = tz
    )
  }

  list(
    tab = masked_tab,
    ok = TRUE,
    masked_rows = bad_idx,
    n_cols = n_cols,
    label_cols = label_cols
  )
}

# Output-Kategorien -----------------------------
## Kategorie-Policy holen --------------------------
get_output_policy <- function(output_category) {
  stopifnot(
    is.character(output_category),
    length(output_category) == 1,
    nzchar(output_category)
  )
  policy <- config$output_categories[[output_category]]
  if (is.null(policy)) {
    stop("Unbekannte output_category: ", output_category)
  }
  policy
}

## Threshold nach Output-Kategorie pruefen --------------------------
passes_threshold_category <- function(
    tab,
    output_category,
    n_col = NULL,
    label_cols = NULL,
    object_name = "object",
    logname = paste0(output_category, "_threshold"),
    write_log = TRUE
) {
  policy <- get_output_policy(output_category)
  if (!isTRUE(policy$threshold)) return(TRUE)

  if (is.null(n_col)) n_col <- policy$n_col
  mode <- if (!is.null(policy$mode)) policy$mode else "any"

  passes_threshold_n_min(
    tab = tab,
    n_col = n_col,
    label_cols = label_cols,
    object_name = object_name,
    min_n = config$thresholds$n_min,
    mode = mode,
    require_count_cols = TRUE,
    logname = logname,
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    write_log = write_log
  )
}

## Threshold nach Output-Kategorie maskieren --------------------------
mask_threshold_category <- function(
    tab,
    output_category,
    n_col = NULL,
    label_cols = NULL,
    object_name = "object",
    logname = paste0(output_category, "_threshold"),
    write_log = TRUE
) {
  policy <- get_output_policy(output_category)
  if (!isTRUE(policy$threshold)) {
    return(list(
      tab = tab,
      ok = TRUE,
      masked_rows = integer(0),
      n_cols = character(0),
      label_cols = character(0)
    ))
  }

  if (is.null(n_col)) n_col <- policy$n_col
  mode <- if (!is.null(policy$mode)) policy$mode else "any"

  mask_threshold_n_min(
    tab = tab,
    n_col = n_col,
    label_cols = label_cols,
    object_name = object_name,
    min_n = config$thresholds$n_min,
    mode = mode,
    require_count_cols = TRUE,
    logname = logname,
    base_dir = file.path(config$paths$current_wd, "output", "logfiles"),
    write_log = write_log
  )
}
