# Diagnosejahr -----------------------------
## Datenbasis --------------------------
year_base <- tumor_base |>
  dplyr::filter(!is.na(diagnosis_year))

# Ausgabe -----------------------------
## Faelle nach Diagnosejahr --------------------------
if (nrow(year_base) == 0L) {
  year_tab <- data.frame(
    diagnosis_year = integer(0),
    n_tumors = integer(0),
    n_patients = integer(0)
  )

  ctx <- tc_write_table(
    tab = year_tab,
    table_id = "02_02_diagnosis_year",
    ctx = ctx,
    output_category = "aggregate_table",
    threshold_n_col = c("n_tumors", "n_patients"),
    threshold_label_cols = "diagnosis_year"
  )

  log_write(
    ctx$log_file,
    "INFO",
    "Diagnosejahresauswertung uebersprungen: keine nicht-fehlenden Diagnosejahre vorhanden."
  )
} else {
  year_tab <- year_base |>
    dplyr::group_by(diagnosis_year) |>
    dplyr::summarise(
      n_tumors = dplyr::n_distinct(LNGTUMORID),
      n_patients = dplyr::n_distinct(LNGREGISTERNR),
      .groups = "drop"
    )

  year_min <- min(year_tab$diagnosis_year, na.rm = TRUE)
  year_max <- max(year_tab$diagnosis_year, na.rm = TRUE)

  if (is.finite(year_min) && is.finite(year_max)) {
    year_tab <- year_tab |>
      tidyr::complete(
        diagnosis_year = seq(year_min, year_max),
        fill = list(n_tumors = 0L, n_patients = 0L)
      )
  }

  ctx <- tc_write_table(
    tab = year_tab,
    table_id = "02_02_diagnosis_year",
    ctx = ctx,
    output_category = "aggregate_table",
    threshold_n_col = c("n_tumors", "n_patients"),
    threshold_label_cols = "diagnosis_year"
  )

  # Grafik -----------------------------
  ## Datenschutzmaskierung --------------------------
  year_plot_tab <- mask_threshold_category(
    tab = year_tab,
    output_category = "aggregate_figure",
    n_col = c("n_tumors", "n_patients"),
    label_cols = "diagnosis_year",
    object_name = "02_02_diagnosis_year_bar"
  )$tab

  if (nrow(year_plot_tab) == 0L || all(is.na(year_plot_tab$n_tumors))) {
    log_write(
      ctx$log_file,
      "INFO",
      "Diagnosejahresgrafik uebersprungen: keine darstellbaren Fallzahlen nach Datenschutzmaskierung."
    )
  } else {
    p_year <- ggplot2::ggplot(
      year_plot_tab,
      ggplot2::aes(x = diagnosis_year, y = n_tumors)
    ) +
      ggplot2::geom_col() +
      ggplot2::labs(
        title = "Tumoren nach Diagnosejahr",
        x = "Diagnosejahr",
        y = "Anzahl Tumoren"
      ) +
      ggplot2::theme_minimal()

    ctx <- tc_write_figure(
      plot = p_year,
      fig_id = "02_02_diagnosis_year",
      filename = "bar",
      ctx = ctx,
      threshold_tab = year_plot_tab,
      output_category = "aggregate_figure",
      threshold_n_col = c("n_tumors", "n_patients"),
      threshold_label_cols = "diagnosis_year"
    )
  }
}
