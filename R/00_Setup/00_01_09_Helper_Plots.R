# Grafik-Helper -----------------------------
## Zweck --------------------------
# Einheitliche Textumbrueche, Plotraender, Farbpaletten und Exportgroessen.
# Die eigentlichen Werte werden zentral in config$figures gepflegt.

# Textumbruch -----------------------------
## Lange Beschriftungen robust umbrechen --------------------------
tc_wrap_plot_text <- function(x, width) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      !is.character(x) ||
      is.na(x[1L]) ||
      !nzchar(x[1L])
  ) {
    return(x)
  }

  lines <- strsplit(x[1L], "\n", fixed = TRUE)[[1L]]
  wrapped <- vapply(
    lines,
    function(line) stringr::str_wrap(line, width = width),
    character(1)
  )

  paste(wrapped, collapse = "\n")
}

# Skalen -----------------------------
## Vorhandene Fill-Skala entfernen --------------------------
tc_remove_fill_scale <- function(plot) {
  fill_scale <- vapply(
    plot$scales$scales,
    function(scale) "fill" %in% scale$aesthetics,
    logical(1)
  )

  if (any(fill_scale)) {
    plot$scales$scales <- plot$scales$scales[!fill_scale]
  }

  plot
}

## Konfigurierte Farbpalette anwenden --------------------------
tc_apply_figure_palette <- function(plot, fig_id, filename) {
  figure_key <- paste(fig_id, filename, sep = "_")

  if (grepl("04_03_area_near_care_bar", figure_key, fixed = TRUE)) {
    palette <- config$figures$palettes$heatmap
    plot <- tc_remove_fill_scale(plot)

    return(
      plot +
        ggplot2::scale_fill_viridis_c(
          option = palette$option,
          begin = palette$begin,
          end = palette$end,
          direction = palette$direction,
          limits = palette$limits,
          na.value = palette$na_value,
          guide = "none"
        )
    )
  }

  if (grepl("near_care_heatmap", figure_key, ignore.case = TRUE)) {
    palette <- config$figures$palettes$heatmap
    plot <- tc_remove_fill_scale(plot)

    return(
      plot +
        ggplot2::scale_fill_viridis_c(
          option = palette$option,
          begin = palette$begin,
          end = palette$end,
          direction = palette$direction,
          limits = palette$limits,
          labels = scales::label_percent(accuracy = 1),
          na.value = palette$na_value,
          name = "Anteil\nwohnortnah"
        )
    )
  }

  if (grepl("choropleth", figure_key, ignore.case = TRUE)) {
    palette <- config$figures$palettes$choropleth
    fill_name <- plot$labels$fill
    fill_name <- if (
      is.null(fill_name) ||
        length(fill_name) == 0L
    ) {
      NULL
    } else {
      as.character(fill_name)[1L]
    }

    plot <- tc_remove_fill_scale(plot)

    return(
      plot +
        ggplot2::scale_fill_viridis_c(
          option = palette$option,
          begin = palette$begin,
          end = palette$end,
          direction = palette$direction,
          na.value = palette$na_value,
          name = fill_name
        )
    )
  }

  if (grepl("age_sex", figure_key, ignore.case = TRUE)) {
    plot <- tc_remove_fill_scale(plot)

    return(
      plot +
        ggplot2::scale_fill_manual(
          values = config$figures$palettes$discrete
        )
    )
  }

  plot
}

# Darstellung -----------------------------
## Stadtbeschriftungen auf Heatmaps --------------------------
# Die bestehenden geom_sf_text-Layer der Stadtmarker werden unmittelbar vor
# dem Export durch helle Labels ersetzt. Dadurch bleiben die Namen auch auf
# dunklen Bereichen der Viridis-Skala lesbar, ohne die Fachskripte mit
# Darstellungsparametern zu belasten.
tc_apply_heatmap_city_labels <- function(plot, fig_id, filename) {
  figure_key <- paste(fig_id, filename, sep = "_")

  if (!grepl("near_care_heatmap", figure_key, ignore.case = TRUE)) {
    return(plot)
  }

  label_layers <- vapply(
    plot$layers,
    function(layer) {
      layer_data <- layer$data
      has_city_data <-
        is.data.frame(layer_data) &&
        "city_label" %in% names(layer_data)
      has_label_mapping <- !is.null(layer$mapping$label)
      has_city_data && has_label_mapping
    },
    logical(1)
  )

  if (!any(label_layers)) {
    return(plot)
  }

  city_label_data <- plot$layers[[which(label_layers)[1L]]]$data
  plot$layers <- plot$layers[!label_layers]

  map_config <- config$figures$maps
  is_stuttgart <- grepl(
    "stuttgart|04_04",
    figure_key,
    ignore.case = TRUE
  )
  label_nudge_y <- if (is_stuttgart) {
    map_config$city_label_nudge_y_stuttgart
  } else {
    map_config$city_label_nudge_y
  }

  plot +
    ggplot2::geom_sf_label(
      data = city_label_data,
      ggplot2::aes(label = city_label),
      nudge_y = label_nudge_y,
      size = map_config$city_label_size,
      fontface = "bold",
      color = map_config$city_label_color,
      fill = map_config$city_label_fill,
      linewidth = map_config$city_label_linewidth,
      label.padding = grid::unit(
        map_config$city_label_padding_lines,
        "lines"
      ),
      label.r = grid::unit(
        map_config$city_label_radius_lines,
        "lines"
      ),
      show.legend = FALSE,
      inherit.aes = FALSE
    )
}

## Einheitliche Texte und Raender --------------------------
tc_apply_figure_style <- function(plot, fig_id, filename) {
  text_config <- config$figures$text
  margins <- text_config$plot_margin_mm
  figure_key <- paste(fig_id, filename, sep = "_")

  plot <- plot +
    ggplot2::labs(
      title = tc_wrap_plot_text(
        plot$labels$title,
        width = text_config$title_wrap
      ),
      subtitle = tc_wrap_plot_text(
        plot$labels$subtitle,
        width = text_config$subtitle_wrap
      ),
      caption = tc_wrap_plot_text(
        plot$labels$caption,
        width = text_config$caption_wrap
      )
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_config$base_size
      ),
      plot.title = ggplot2::element_text(
        size = text_config$title_size,
        face = "bold",
        lineheight = text_config$lineheight,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = text_config$subtitle_size,
        lineheight = text_config$lineheight,
        margin = ggplot2::margin(b = 6)
      ),
      plot.caption = ggplot2::element_text(
        size = text_config$caption_size,
        lineheight = text_config$lineheight,
        margin = ggplot2::margin(t = 6)
      ),
      axis.title = ggplot2::element_text(
        size = text_config$axis_title_size
      ),
      legend.title = ggplot2::element_text(
        size = text_config$legend_title_size,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        size = text_config$legend_text_size
      ),
      plot.margin = ggplot2::margin(
        t = unname(margins["top"]),
        r = unname(margins["right"]),
        b = unname(margins["bottom"]),
        l = unname(margins["left"]),
        unit = "mm"
      )
    )

  # Karten-Heatmaps sollen keine von ggplot2 automatisch erzeugten
  # x-/y-Achsentitel oder Koordinatenachsen anzeigen. Dieser Schritt steht
  # bewusst nach dem allgemeinen Theme, damit axis.title nicht erneut gesetzt wird.
  if (grepl("near_care_heatmap", figure_key, ignore.case = TRUE)) {
    plot <- plot +
      ggplot2::labs(
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )

    plot <- tc_apply_heatmap_city_labels(
      plot = plot,
      fig_id = fig_id,
      filename = filename
    )
  }

  tc_apply_figure_palette(
    plot = plot,
    fig_id = fig_id,
    filename = filename
  )
}

# Exportgroesse -----------------------------
## Passendes Groessenpreset bestimmen --------------------------
tc_figure_export_settings <- function(fig_id, filename) {
  export_config <- config$figures$export
  figure_key <- paste(fig_id, filename, sep = "_")
  preset_name <- "default"

  for (candidate in names(export_config$preset_rules)) {
    patterns <- export_config$preset_rules[[candidate]]
    is_match <- any(
      vapply(
        patterns,
        function(pattern) grepl(pattern, figure_key, ignore.case = TRUE),
        logical(1)
      )
    )

    if (is_match) {
      preset_name <- candidate
      break
    }
  }

  preset <- export_config$presets[[preset_name]]

  list(
    preset = preset_name,
    width = preset$width,
    height = preset$height,
    dpi = export_config$dpi,
    background = export_config$background
  )
}

# Grafikexport -----------------------------
## PNG und PDF mit identischer Darstellungslogik --------------------------
# Diese Definitionen werden nach 00_01_03_Helper_Write.R geladen und ersetzen
# gezielt nur die beiden Grafikfunktionen. Tabellen- und Threshold-Logik bleiben
# unveraendert in ihrem bisherigen Helper.
tc_save_figure <- function(
    plot,
    fig_id,
    filename,
    paths,
    log_file,
    width = NULL,
    height = NULL,
    dpi = NULL
) {
  export_settings <- tc_figure_export_settings(
    fig_id = fig_id,
    filename = filename
  )

  width <- if (is.null(width)) export_settings$width else width
  height <- if (is.null(height)) export_settings$height else height
  dpi <- if (is.null(dpi)) export_settings$dpi else dpi

  stem <- paste0(fig_id, "_", filename)
  png_path <- file.path(paths$figures, paste0(stem, ".png"))
  pdf_path <- file.path(paths$figures, paste0(stem, ".pdf"))

  png_device <- if (
    isTRUE(config$figures$export$use_ragg_if_available) &&
      requireNamespace("ragg", quietly = TRUE)
  ) {
    ragg::agg_png
  } else {
    "png"
  }

  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    device = png_device,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = export_settings$background,
    limitsize = FALSE
  )

  if (
    isTRUE(config$figures$export$use_cairo_pdf_if_available) &&
      isTRUE(capabilities("cairo"))
  ) {
    ggplot2::ggsave(
      filename = pdf_path,
      plot = plot,
      device = grDevices::cairo_pdf,
      width = width,
      height = height,
      units = "in",
      bg = export_settings$background,
      limitsize = FALSE
    )
  } else {
    ggplot2::ggsave(
      filename = pdf_path,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      bg = export_settings$background,
      limitsize = FALSE
    )
  }

  log_write(
    log_file,
    "INFO",
    "Figure written: ",
    stem,
    " | preset=",
    export_settings$preset,
    " | ",
    width,
    "x",
    height,
    " in"
  )

  invisible(list(png = png_path, pdf = pdf_path))
}

## Grafik mit Threshold schreiben --------------------------
tc_write_figure <- function(
    plot,
    fig_id,
    filename,
    ctx,
    threshold_tab,
    output_category = "aggregate_figure",
    threshold = TRUE,
    threshold_n_col = NULL,
    threshold_label_cols = NULL,
    threshold_object_name = paste0(fig_id, "_", filename)
) {
  stopifnot(is.data.frame(threshold_tab))
  ok <- TRUE

  if (isTRUE(threshold)) {
    ok <- passes_threshold_category(
      tab = threshold_tab,
      output_category = output_category,
      n_col = threshold_n_col,
      label_cols = threshold_label_cols,
      object_name = threshold_object_name
    )
  }

  if (isTRUE(ok)) {
    plot <- tc_add_analysis_label_to_plot(
      plot,
      ctx$analysis_label
    )
    plot <- tc_apply_figure_style(
      plot = plot,
      fig_id = fig_id,
      filename = filename
    )

    tc_save_figure(
      plot = plot,
      fig_id = fig_id,
      filename = filename,
      paths = ctx$paths,
      log_file = ctx$log_file
    )
  } else {
    ctx <- tc_add_suppressed(
      ctx = ctx,
      object_id = paste0(fig_id, "_", filename),
      output_category = output_category,
      reason = paste0(
        "Datenschutz-Threshold verletzt: 0 < n < ",
        config$thresholds$n_min
      )
    )
  }

  ctx
}
