# Demografie -----------------------------
## Altersgruppen und Geschlecht --------------------------
# Die Deskription kann mehrere Altersklassenschemata parallel ausgeben.
# Ausserhalb der Deskription bleibt config$explore$active_age_scheme massgeblich.

age_scheme_names <- if (isTRUE(config$explore$run_all_age_schemes_descriptive)) {
  names(config$explore$age_schemes)
} else {
  config$explore$active_age_scheme
}

for (age_scheme_name in age_scheme_names) {
  age_scheme <- config$explore$age_schemes[[age_scheme_name]]

  # Altersgruppen bilden -----------------------------
  ## Aktuelles Schema anwenden --------------------------
  demo_tab <- tumor_base |>
    dplyr::mutate(
      sex = dplyr::if_else(is.na(sex), "unbekannt", sex),
      age_group = cut(
        as.numeric(age_at_diag),
        breaks = age_scheme$breaks,
        labels = age_scheme$labels,
        right = FALSE
      )
    ) |>
    dplyr::group_by(age_group, sex) |>
    dplyr::summarise(
      n_tumors = dplyr::n_distinct(LNGTUMORID),
      n_patients = dplyr::n_distinct(LNGREGISTERNR),
      .groups = "drop"
    ) |>
    tidyr::complete(
      age_group = factor(age_scheme$labels, levels = age_scheme$labels),
      sex,
      fill = list(n_tumors = 0L, n_patients = 0L)
    )

  # Tabelle -----------------------------
  ## Datenschutzbewusst schreiben --------------------------
  table_id <- paste0("02_03_age_sex_", age_scheme_name)

  ctx <- tc_write_table(
    tab = demo_tab,
    table_id = table_id,
    ctx = ctx,
    output_category = "crosstab",
    threshold_n_col = c("n_tumors", "n_patients"),
    threshold_label_cols = c("age_group", "sex")
  )

  # Grafik -----------------------------
  ## Threshold anwenden und Balkendiagramm erstellen --------------------------
  demo_plot_tab <- mask_threshold_category(
    tab = demo_tab,
    output_category = "aggregate_figure",
    n_col = c("n_tumors", "n_patients"),
    label_cols = c("age_group", "sex"),
    object_name = paste0(table_id, "_stacked_bar")
  )$tab

  p_demo <- ggplot2::ggplot(
    demo_plot_tab,
    ggplot2::aes(x = age_group, y = n_tumors, fill = sex)
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::labs(
      title = paste0(
        "Tumoren nach Altersgruppe und Geschlecht - ",
        age_scheme$label
      ),
      x = "Altersgruppe",
      y = "Anzahl Tumoren",
      fill = "Geschlecht"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  ctx <- tc_write_figure(
    plot = p_demo,
    fig_id = table_id,
    filename = "stacked_bar",
    ctx = ctx,
    threshold_tab = demo_plot_tab,
    output_category = "aggregate_figure",
    threshold_n_col = c("n_tumors", "n_patients"),
    threshold_label_cols = c("age_group", "sex")
  )
}
