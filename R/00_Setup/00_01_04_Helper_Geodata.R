# 00_01_04_Helper_Geodata.R
#
# Zentrale Funktionen zum Aufloesen, Laden, Verbinden und Darstellen
# optionaler Geodaten. Fehlt eine Datei, werden nur die betroffenen Karten
# uebersprungen und der Grund im Analyse-Log dokumentiert.


# Geodatenpfade -----------------------------
## Projektpfad aufloesen --------------------------

resolve_geodata_path <- function(
    path,
    current_wd = config$paths$current_wd
) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    !is.na(path),
    nzchar(path),
    is.character(current_wd),
    length(current_wd) == 1L,
    !is.na(current_wd),
    nzchar(current_wd)
  )

  # Windows-Laufwerk, UNC-Pfad oder Unix-Wurzel erkennen.
  is_absolute <- grepl(
    pattern = "^([A-Za-z]:[/\\\\]|[/\\\\]{2}|/)",
    x = path
  )

  resolved_path <- if (is_absolute) {
    path
  } else {
    file.path(
      current_wd,
      path
    )
  }

  normalizePath(
    resolved_path,
    winslash = "/",
    mustWork = FALSE
  )
}


# Geometrien -----------------------------
## Optionales Laden --------------------------

load_sf_optional <- function(
    path,
    layer = NULL,
    crs_target = config$geodata$crs_target,
    log_file = NULL
) {
  if (
    is.null(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !file.exists(path)
  ) {
    if (!is.null(log_file)) {
      log_write(
        log_file,
        "WARN",
        "Geodaten nicht gefunden: ",
        path
      )
    }

    return(NULL)
  }

  sf_obj <- if (is.null(layer)) {
    sf::st_read(
      path,
      quiet = TRUE
    )
  } else {
    sf::st_read(
      path,
      layer = layer,
      quiet = TRUE
    )
  }

  if (
    !is.na(sf::st_crs(sf_obj)) &&
      !is.null(crs_target)
  ) {
    sf_obj <- sf::st_transform(
      sf_obj,
      crs = crs_target
    )
  }

  sf_obj
}


## Tabellarischer Join --------------------------

join_geo_counts <- function(
    sf_obj,
    count_tab,
    sf_key,
    tab_key
) {
  stopifnot(
    inherits(sf_obj, "sf"),
    is.data.frame(count_tab),
    sf_key %in% names(sf_obj),
    tab_key %in% names(count_tab)
  )

  sf_obj[[sf_key]] <- kr_normalize_gkz(
    sf_obj[[sf_key]]
  )

  count_tab[[tab_key]] <- kr_normalize_gkz(
    count_tab[[tab_key]]
  )

  dplyr::left_join(
    sf_obj,
    count_tab,
    by = stats::setNames(
      tab_key,
      sf_key
    )
  )
}


# Karten -----------------------------
## Choroplethenkarte --------------------------

plot_choropleth <- function(
    sf_obj,
    fill_var,
    title = NULL
) {
  stopifnot(
    inherits(sf_obj, "sf"),
    fill_var %in% names(sf_obj)
  )

  ggplot2::ggplot(
    sf_obj
  ) +
    ggplot2::geom_sf(
      ggplot2::aes(
        fill = .data[[fill_var]]
      ),
      color = NA
    ) +
    ggplot2::coord_sf(
      datum = NA
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      na.value = "grey90"
    ) +
    ggplot2::labs(
      title = title,
      fill = fill_var
    ) +
    ggplot2::theme_minimal()
}
