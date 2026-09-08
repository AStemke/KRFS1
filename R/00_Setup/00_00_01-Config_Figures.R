# Grafik-Konfiguration -----------------------------
## Zweck --------------------------
# Zentrale Darstellungs- und Exporteinstellungen fuer alle Abbildungen.
# Fachliche Daten-/Analyseparameter bleiben in 00_00-Config.R bzw. den
# jeweiligen Modul-Setups; hier stehen nur visuelle und Export-Parameter.

krfs1_figure_config <- list(
  # Export -----------------------------
  ## Groessen und Dateiausgabe --------------------------
  export = list(
    dpi = 450,
    background = "white",
    use_ragg_if_available = TRUE,
    use_cairo_pdf_if_available = TRUE,
    # JPEG wird zunaechst nur fuer Heatmaps als Vergleichsformat ausgegeben.
    write_jpeg = TRUE,
    jpeg_quality = 95,
    jpeg_patterns = c("heatmap"),
    presets = list(
      default = list(width = 11, height = 6.5),
      wide = list(width = 12, height = 6.5),
      map = list(width = 11.5, height = 8)
    ),
    preset_rules = list(
      map = c("heatmap", "choropleth"),
      wide = c("age_sex", "demograph")
    )
  ),

  # Text -----------------------------
  ## Schrift und Umbruch --------------------------
  text = list(
    base_size = 11,
    title_size = 15,
    subtitle_size = 11,
    caption_size = 9,
    axis_title_size = 10.5,
    legend_title_size = 10.5,
    legend_text_size = 9.5,
    # Etwas breitere Zeilen vermeiden unguenstige fruehe Umbrueche.
    title_wrap = 66L,
    subtitle_wrap = 88L,
    caption_wrap = 100L,
    lineheight = 1.08,
    plot_margin_mm = c(top = 10, right = 12, bottom = 10, left = 12)
  ),

  # Farbpaletten -----------------------------
  ## Allgemeine und kartenspezifische Farben --------------------------
  palettes = list(
    primary = "#2C7FB8",
    discrete = c(
      "#0072B2", "#E69F00", "#009E73", "#CC79A7",
      "#56B4E9", "#D55E00", "#F0E442", "#000000"
    ),
    heatmap = list(
      # Viridis-Familie; option = "C" behaelt die bisherige Farbskala bei.
      option = "C",
      begin = 0.05,
      end = 0.95,
      direction = 1,
      limits = c(0, 1),
      na_value = "grey90"
    ),
    choropleth = list(
      option = "C",
      begin = 0.05,
      end = 0.95,
      direction = 1,
      na_value = "grey90"
    )
  ),

  # Karten -----------------------------
  ## Linien, Punkte und Beschriftungen --------------------------
  maps = list(
    boundary_color = "grey70",
    line_width_by_digits = c(
      "5" = 0.15,
      "8" = 0.025
    ),
    city_point_size = 2.4,
    city_point_stroke = 0.6,
    city_label_size = 2.8,
    # Staedtenamen etwas oberhalb der Punkte platzieren.
    city_label_nudge_y = 7500,
    city_label_nudge_y_stuttgart = 1400,
    # Heller Label-Hintergrund sorgt auch auf dunkler Heatmap fuer Lesbarkeit.
    city_label_color = "black",
    city_label_fill = "white",
    city_label_linewidth = 0.15,
    city_label_padding_lines = 0.12,
    city_label_radius_lines = 0.08
  )
)

# Rueckwaertskompatibilitaet -----------------------------
## Bisherige Direktzugriffe erhalten --------------------------
krfs1_figure_config$width <- krfs1_figure_config$export$presets$default$width
krfs1_figure_config$height <- krfs1_figure_config$export$presets$default$height
krfs1_figure_config$dpi <- krfs1_figure_config$export$dpi
