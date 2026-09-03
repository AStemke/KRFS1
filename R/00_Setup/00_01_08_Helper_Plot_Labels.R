# Abbildungsbeschriftungen -----------------------------
## Kohorte, Region und Distanzgrenze --------------------------
# Erweitert die bestehende Beschriftungslogik. Alle Abbildungen erhalten bei
# aktiviertem Regionalfilter die aktive Analyseregion. Bei Abbildungen zur
# wohnortnahen Versorgung wird zusaetzlich die tatsaechlich verwendete
# Distanzkonfiguration im Titel genannt.

tc_add_analysis_label_to_plot <- function(plot, analysis_label) {
  current_title <- plot$labels$title
  current_title <- if (
    is.null(current_title) || length(current_title) == 0L
  ) {
    ""
  } else {
    as.character(current_title)[1L]
  }

  current_subtitle <- plot$labels$subtitle
  current_subtitle <- if (
    is.null(current_subtitle) || length(current_subtitle) == 0L
  ) {
    ""
  } else {
    as.character(current_subtitle)[1L]
  }

  # Nur Abbildungen zur wohnortnahen Versorgung erhalten die Distanz im Titel.
  if (grepl("wohnortnah", current_title, ignore.case = TRUE)) {
    distance_label <- kr_near_care_distance_label()

    if (!grepl("Distanz:", current_title, fixed = TRUE)) {
      current_title <- paste0(
        current_title,
        " | Distanz: ",
        distance_label
      )
    }

    # Veraltete globale Distanzangaben aus bestehenden Untertiteln entfernen.
    current_subtitle <- gsub(
      "\\s*\\|\\s*Wohnortnah bis [^|]+ km(?=\\s*\\||$)",
      "",
      current_subtitle,
      perl = TRUE
    )
    current_subtitle <- gsub(
      "\\s*\\|\\s*Default-Distanz.*$",
      "",
      current_subtitle,
      perl = TRUE
    )
    current_subtitle <- trimws(current_subtitle)
  }

  # Die aktive Analyseregion wird in allen Abbildungen sichtbar dokumentiert.
  if (exists("kr_analysis_region_label", mode = "function")) {
    region_label <- kr_analysis_region_label()
    region_already_shown <- nzchar(region_label) && (
      grepl(region_label, current_title, fixed = TRUE) ||
        grepl(region_label, current_subtitle, fixed = TRUE)
    )

    if (nzchar(region_label) && !region_already_shown) {
      current_subtitle <- if (nzchar(current_subtitle)) {
        paste(current_subtitle, region_label, sep = " | ")
      } else {
        region_label
      }
    }
  }

  title_prefix <- paste0(analysis_label, ": ")

  title_with_label <- if (!nzchar(trimws(current_title))) {
    analysis_label
  } else if (
    identical(current_title, analysis_label) ||
      startsWith(current_title, title_prefix)
  ) {
    current_title
  } else {
    paste0(title_prefix, current_title)
  }

  plot +
    ggplot2::labs(
      title = title_with_label,
      subtitle = if (nzchar(current_subtitle)) current_subtitle else NULL
    )
}
