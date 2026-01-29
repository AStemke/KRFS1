# 00_02_01_Data_Setup_Helper

# =============================================================================
# load_ads.R
# Zweck:
# - Lädt Dummy- oder Productive-Datensatz aus data/<Mode>/raw
# - Cacht als RDS in data/<Mode>/ads
# - Liest Excel IMMER als Text ein
# - Leitet Typen heuristisch aus Legende (Variable/Bedeutung) ab
# - Erstellt Faktoren inkl. Levels/Labels aus Kodierungen in Bedeutung ("m=...", "9=...")
# - Setzt Distanz_m / Distanz_km als numeric
#
# Abhängigkeiten: readxl, tibble, dplyr, readr, lubridate
# =============================================================================

# ---- helpers ----------------------------------------------------------------

.as_lower <- function(x) {
  tolower(trimws(as.character(x)))
}


## Extract Factors -------------------------------------------------------

# Extrahiert Kodierungen aus Bedeutung:
# Unterstützt Zeilen wie "m= männlich" oder "9 = unbekannt"
extract_codelist <- function(text) {
  x <- as.character(text)

  if (is.na(x) || trimws(x) == "") {
    return(tibble::tibble(code = character(0), label = character(0)))
  }

  # Zeilenumbrüche vereinheitlichen
  x <- gsub("\r\n|\r", "\n", x)

  lines <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]

  # Muster: (code)=(label)
  m <- regexec("^([^=]{1,40})\\s*=\\s*(.+)$", lines, perl = TRUE)
  mm <- regmatches(lines, m)

  if (length(mm) == 0) {
    return(tibble::tibble(code = character(0), label = character(0)))
  }

  codes  <- vapply(mm, function(z) trimws(z[2]), character(1))
  labels <- vapply(mm, function(z) trimws(z[3]), character(1))

  keep <- nzchar(codes) & nzchar(labels)
  tibble::tibble(code = codes[keep], label = labels[keep])
}

has_codelist <- function(text) {
  nrow(extract_codelist(text)) > 0
}


## Parsing Helpers -------------------------------------------------------

parse_logical_de <- function(x) {
  x0 <- .as_lower(x)
  dplyr::case_when(
    is.na(x0) | x0 == "" ~ NA,
    x0 %in% c("1", "t", "true", "ja", "j", "yes", "y") ~ TRUE,
    x0 %in% c("0", "f", "false", "nein", "n", "no") ~ FALSE,
    TRUE ~ NA
  )
}

parse_numeric_de <- function(x) {
  readr::parse_number(
    x,
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
}

parse_distance_strict <- function(x) {
  x0 <- trimws(as.character(x))
  x0[x0 == ""] <- NA_character_
  
  # deutsches Dezimalkomma → Punkt
  x0 <- gsub(",", ".", x0, fixed = TRUE)
  
  # alles außer Ziffern, Punkt, Minus entfernen
  x0 <- gsub("[^0-9.-]", "", x0)
  
  suppressWarnings(as.numeric(x0))
}


parse_integer_de <- function(x) {
  as.integer(parse_numeric_de(x))
}

parse_date_month_year <- function(x) {
  # "05.2021" -> 2021-05-01
  x0 <- trimws(as.character(x))
  ok <- grepl("^\\d{2}\\.\\d{4}$", x0)

  out <- rep(as.Date(NA), length(x0))
  out[ok] <- as.Date(paste0(substr(x0[ok], 4, 7), "-", substr(x0[ok], 1, 2), "-01"))
  out
}

parse_datetime_safely <- function(x) {
  x0 <- trimws(as.character(x))

  y <- suppressWarnings(lubridate::ymd_hms(x0, quiet = TRUE))
  miss <- is.na(y) & !is.na(x0) & x0 != ""

  if (any(miss)) {
    y2 <- suppressWarnings(lubridate::dmy_hms(x0[miss], quiet = TRUE))
    y[miss] <- y2
  }

  y
}


##Faktoren aus der Legende erstellen -------------------------------------
# Legende: erwartet Spalten "Variable" und "Bedeutung" 
normalize_legend_vb <- function(legend) {
  stopifnot(is.data.frame(legend))

  nms <- names(legend)

  var_col <- nms[match(TRUE, tolower(nms) %in% c("variable", "variablenname", "spalte", "feld"), nomatch = NA_integer_)]
  if (is.na(var_col)) var_col <- "Variable"

  bed_col <- nms[match(TRUE, tolower(nms) %in% c("bedeutung", "beschreibung", "label"), nomatch = NA_integer_)]
  if (is.na(bed_col)) bed_col <- "Bedeutung"

  legend |>
    dplyr::transmute(
      variable  = trimws(as.character(.data[[var_col]])),
      bedeutung = dplyr::if_else(is.na(.data[[bed_col]]), "", as.character(.data[[bed_col]]))
    ) |>
    dplyr::filter(!is.na(variable), variable != "")
}

# Typen aus Variablenname-Präfix + Kodierungen in Bedeutung ableiten
infer_schema_from_legend <- function(legend_norm) {
  stopifnot(all(c("variable", "bedeutung") %in% names(legend_norm)))

  legend_norm |>
    dplyr::mutate(
      prefix = substr(variable, 1, 3),
      code_like = vapply(bedeutung, has_codelist, logical(1)),
      type = dplyr::case_when(
        # harte Overrides
        variable %in% c("Distanz_m", "Distanz_km") ~ "numeric",

        # Codes als character (niemals numeric parsen)
        grepl("^(PLZ|GKZ)_", variable) ~ "character",

        # Zeit/Datum
        prefix == "DTM" ~ "datetime",

        # Flag
        prefix == "DFL" ~ "logical",

        # Long IDs
        prefix == "LNG" ~ "integer",

        # STR: wenn Kodierungen drin -> factor, sonst character
        prefix == "STR" & code_like ~ "factor",
        prefix == "STR" ~ "character",

        TRUE ~ "character"
      )
    ) |>
    dplyr::select(variable, bedeutung, type)
}

# Wendet Schema an: Text -> typed + Faktoren inkl. Levels/Labels aus Bedeutung
apply_schema <- function(data_txt, schema) {
  stopifnot(is.data.frame(data_txt), is.data.frame(schema))
  stopifnot(all(c("variable", "type") %in% names(schema)))

  out <- data_txt

  # Labels als Attribute (für Doku)
  attr(out, "var_labels") <- stats::setNames(schema$bedeutung, schema$variable)

  for (i in seq_len(nrow(schema))) {
    v <- schema$variable[i]
    t <- schema$type[i]

    if (!v %in% names(out)) next

    x <- out[[v]]
    
    # Distanz: niemals parse_number(), sonst Exponential-Müll
    if (v %in% c("Distanz_m", "Distanz_km")) {
      out[[v]] <- parse_distance_strict(x)
      next
    }

    # Spezialfall: "05.2021"
    if (v == "DIAGNOSEDATUM") {
      out[[v]] <- parse_date_month_year(x)
      next
    }

    if (t == "integer") {
      out[[v]] <- parse_integer_de(x)

    } else if (t == "numeric") {
      out[[v]] <- parse_numeric_de(x)

    } else if (t == "logical") {
      out[[v]] <- parse_logical_de(x)

    } else if (t == "datetime") {
      out[[v]] <- parse_datetime_safely(x)

    } else if (t == "factor") {
      cl0 <- extract_codelist(schema$bedeutung[i])

      # keine Codes gefunden -> einfacher factor
      if (nrow(cl0) == 0) {
        out[[v]] <- as.factor(x)

      } else {
        # 1) deduplizieren (pro code erster Eintrag; case-insensitive)
        cl <- cl0 |>
          dplyr::mutate(code_low = .as_lower(code)) |>
          dplyr::group_by(code_low) |>
          dplyr::slice(1) |>
          dplyr::ungroup()

        # 2) Datenwerte auf canonical codes mappen (case-insensitive)
        x_chr <- as.character(x)
        x_low <- .as_lower(x_chr)

        canon_code_by_low <- stats::setNames(cl$code, cl$code_low)

        canon_codes <- x_chr
        hit <- x_low %in% names(canon_code_by_low)
        canon_codes[hit] <- unname(canon_code_by_low[x_low[hit]])

        # 3) Levels/Labels aus Legende + unknown codes
        lev_codes  <- cl$code
        lev_labels <- cl$label

        unknown_codes <- sort(unique(canon_codes[
          !is.na(canon_codes) & canon_codes != "" & !(canon_codes %in% lev_codes)
        ]))

        lev_codes2  <- c(lev_codes, unknown_codes)
        lev_labels2 <- c(lev_labels, unknown_codes)
        
        lev_levels <- tibble::tibble(code = lev_codes2, label = lev_labels2) |>
          dplyr::filter(!is.na(code), code != "") |>
          dplyr::distinct(code, .keep_all = TRUE)
        
        lev_codes2  <- lev_levels$code
        lev_labels2 <- lev_levels$label

        # 4) Sicherheitscheck (labels muss exakt zu levels passen)
        if (length(lev_codes2) != length(lev_labels2)) {
          warning(
            "Factor mapping inkonsistent für Variable: ", v,
            " (levels=", length(lev_codes2), ", labels=", length(lev_labels2), "). ",
            "Fallback: labels = levels."
          )
          lev_labels2 <- lev_codes2
        }

        out[[v]] <- factor(canon_codes, levels = lev_codes2, labels = lev_labels2)

        # optional: Codes als Attribut
        attr(out[[v]], "code_levels") <- lev_codes2
      }

    } else {
      out[[v]] <- as.character(x)
    }
  }

  out
}

# ---- main loader ------------------------------------------------------------

load_ads <- function(current_wd, productive = FALSE) {
  stopifnot(is.logical(productive), length(productive) == 1)

  mode <- if (productive) "Productive" else "Dummy"
  raw_dir_rel <- file.path("data", mode, "raw")
  ads_dir_rel <- file.path("data", mode, "ads")
  rds_path <- file.path(current_wd, ads_dir_rel, paste0("ads_list_", tolower(mode), ".rds"))

  if (file.exists(rds_path)) {
    message(mode, "-ADS gefunden → lade: ", rds_path)
    return(readRDS(rds_path))
  }

  raw_dir <- file.path(current_wd, raw_dir_rel)
  ads_dir <- file.path(current_wd, ads_dir_rel)

  if (!dir.exists(ads_dir)) {
    dir.create(ads_dir, recursive = TRUE)
  }

  xlsx <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)

  legend_files <- xlsx[grepl("_Legende\\.xlsx$", basename(xlsx), ignore.case = TRUE)]
  data_files   <- xlsx[!grepl("_Legende\\.xlsx$", basename(xlsx), ignore.case = TRUE)]

  if (length(data_files) == 0) stop("Keine Datensatz-Datei gefunden in: ", raw_dir)
  if (length(legend_files) == 0) stop("Keine Legende-Datei gefunden in: ", raw_dir)

  # jeweils neueste Version
  data_file <- data_files[which.max(file.info(data_files)$mtime)]
  legend_file <- legend_files[which.max(file.info(legend_files)$mtime)]

  message("Lese ", mode, "-Daten:   ", basename(data_file))
  message("Lese ", mode, "-Legende: ", basename(legend_file))

  # 1) immer als TEXT einlesen
  data_txt <- readxl::read_excel(data_file, col_types = "text") |> tibble::as_tibble()
  legend_txt <- readxl::read_excel(legend_file, col_types = "text") |> tibble::as_tibble()

  # 2) Legende -> Schema -> Typisierung/Faktoren
  legend_norm <- normalize_legend_vb(legend_txt)
  schema <- infer_schema_from_legend(legend_norm)
  data_typed <- apply_schema(data_txt, schema)

  meta <- list(
    productive = productive,
    mode = mode,
    source_data_file = basename(data_file),
    source_legend_file = basename(legend_file),
    created_at = Sys.time(),
    raw_dir = raw_dir_rel,
    ads_dir = ads_dir_rel
  )

  ads_list <- list(
    data = data_typed,
    legend = schema,  # variable, bedeutung, type
    meta = meta
  )

  saveRDS(ads_list, rds_path)
  message(mode, "-ADS gespeichert: ", rds_path)

  ads_list
}
