# 00_01_04_Helper_Geodata.R -----------
# Zweck:
# - Zentrale Funktionen fuer Geodaten
# - Geodaten sind optional: Fehlen Dateien, werden Karten uebersprungen und geloggt

load_sf_optional <- function(path, layer = NULL, crs_target = config$geodata$crs_target, log_file = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    if (!is.null(log_file)) log_write(log_file, "WARN", "Geodaten nicht gefunden: ", path)
    return(NULL)
  }
  
  x <- if (is.null(layer)) {
    sf::st_read(path, quiet = TRUE)
  } else {
    sf::st_read(path, layer = layer, quiet = TRUE)
  }
  
  if (!is.na(sf::st_crs(x)) && !is.null(crs_target)) {
    x <- sf::st_transform(x, crs = crs_target)
  }
  x
}

join_geo_counts <- function(sf_obj, count_tab, sf_key, tab_key) {
  stopifnot(inherits(sf_obj, "sf"), is.data.frame(count_tab))
  sf_obj[[sf_key]] <- kr_normalize_gkz(sf_obj[[sf_key]])
  count_tab[[tab_key]] <- kr_normalize_gkz(count_tab[[tab_key]])
  dplyr::left_join(sf_obj, count_tab, by = stats::setNames(tab_key, sf_key))
}

plot_choropleth <- function(sf_obj, fill_var, title = NULL) {
  stopifnot(inherits(sf_obj, "sf"), fill_var %in% names(sf_obj))
  ggplot2::ggplot(sf_obj) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_var]]), color = NA) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_viridis_c(option = "C", na.value = "grey90") +
    ggplot2::labs(title = title, fill = fill_var) +
    ggplot2::theme_minimal()
}
