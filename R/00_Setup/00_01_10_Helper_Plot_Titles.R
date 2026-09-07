# Abbildungstitel -----------------------------
## Wohnortnahe Erstdiagnosen --------------------------
# Vereinheitlicht die sichtbare Bezeichnung der Near-Care-Abbildungen. Die
# fachlichen Plot-Skripte koennen ihre dynamischen Titel weiterhin selbst
# zusammensetzen; unmittelbar vor dem Export wird nur die feste Formulierung
# "wohnortnaher Versorgung" durch "wohnortnaher Erstdiagnosen" ersetzt.

if (!exists("tc_apply_figure_style_core", inherits = FALSE)) {
  tc_apply_figure_style_core <- tc_apply_figure_style
}

tc_apply_figure_style <- function(plot, fig_id, filename) {
  figure_key <- paste(fig_id, filename, sep = "_")

  is_near_care_title <-
    grepl("near_care_heatmap", figure_key, ignore.case = TRUE) ||
    grepl("04_03_area_near_care_bar", figure_key, fixed = TRUE)

  if (is_near_care_title) {
    current_title <- plot$labels$title

    if (
      !is.null(current_title) &&
        length(current_title) > 0L &&
        !is.na(current_title[1L])
    ) {
      plot <- plot +
        ggplot2::labs(
          title = gsub(
            "wohnortnaher Versorgung",
            "wohnortnaher Erstdiagnosen",
            as.character(current_title)[1L],
            fixed = TRUE
          )
        )
    }
  }

  tc_apply_figure_style_core(
    plot = plot,
    fig_id = fig_id,
    filename = filename
  )
}
