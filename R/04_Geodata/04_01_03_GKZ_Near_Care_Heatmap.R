# Heatmap wohnortnahe Versorgung -----------------------------
## Zweck --------------------------
# Vorbereitung und Prüfung des Joins zwischen Tumordaten
# und Gemeindegeometrien.
#
# Dieses Skript endet für den ersten Test absichtlich
# nach der Join-Diagnostik.

# Hilfsfunktionen -----------------------------
## Gemeindeschlüssel vereinheitlichen --------------------------

normalize_gkz <- function(x) {
  
  x <- as.character(x)
  x <- trimws(x)
  
  # Eventuelle Dezimalstellen numerisch importierter GKZ entfernen
  x <- sub("\\.0+$", "", x)
  
  # Amtlichen Gemeindeschlüssel auf acht Stellen auffüllen
  x <- stringr::str_pad(
    string = x,
    width = 8L,
    side = "left",
    pad = "0"
  )
  
  x[x == "00000000"] <- NA_character_
  
  x
}

# Zielkohorten -----------------------------
## Heatmap nur für selektive und fokussierte Auswertungen --------------------------

heatmap_groups <- c(
  "Selektiv_Gesamt",
  config$analysis$focus_icd10_3
)

if (config$analysis$active_output_group %in% heatmap_groups) {
  
  # Eingangsdaten prüfen -----------------------------
  ## Erforderliche Objekte und Spalten --------------------------
  
  stopifnot(
    exists("tumor_base"),
    exists("gkz_geo"),
    inherits(gkz_geo, "sf"),
    "gkz_diagnose" %in% names(tumor_base),
    "distance_km" %in% names(tumor_base),
    "near_care" %in% names(tumor_base),
    "gemeinde_i" %in% names(gkz_geo)
  )
  
  # Geodaten vorbereiten -----------------------------
  ## Gemeindeschlüssel der Shape-Datei normalisieren --------------------------
  
  gkz_geo_heatmap <- gkz_geo |>
    dplyr::mutate(
      gkz_join = normalize_gkz(gemeinde_i)
    )
  
  # Tumordaten vorbereiten -----------------------------
  ## Diagnose-GKZ normalisieren --------------------------
  
  tumor_heatmap_base <- tumor_base |>
    dplyr::mutate(
      gkz_join = normalize_gkz(gkz_diagnose)
    ) |>
    dplyr::filter(
      !is.na(gkz_join),
      !is.na(distance_km),
      !is.na(near_care)
    )
  
  # Aggregation -----------------------------
  ## Wohnortnahe Versorgung nach Diagnose-GKZ --------------------------
  
  gkz_near_care_tab <- tumor_heatmap_base |>
    dplyr::group_by(gkz_join) |>
    dplyr::summarise(
      n_tumors = dplyr::n(),
      n_near = sum(near_care),
      n_not_near = sum(!near_care),
      share_near = n_near / n_tumors,
      .groups = "drop"
    )
  # Join-Diagnostik -----------------------------
  ## Nicht kartierbare Diagnose-GKZ bestimmen --------------------------
  
  bundesland_lookup <- c(
    "01" = "Schleswig-Holstein",
    "02" = "Hamburg",
    "03" = "Niedersachsen",
    "04" = "Bremen",
    "05" = "Nordrhein-Westfalen",
    "06" = "Hessen",
    "07" = "Rheinland-Pfalz",
    "08" = "Baden-Württemberg",
    "09" = "Bayern",
    "10" = "Saarland",
    "11" = "Berlin",
    "12" = "Brandenburg",
    "13" = "Mecklenburg-Vorpommern",
    "14" = "Sachsen",
    "15" = "Sachsen-Anhalt",
    "16" = "Thüringen"
  )
  
  gkz_join_check <- gkz_near_care_tab |>
    dplyr::anti_join(
      gkz_geo_heatmap |>
        sf::st_drop_geometry() |>
        dplyr::distinct(gkz_join),
      by = "gkz_join"
    ) |>
    dplyr::mutate(
      bundesland_id = substr(
        gkz_join,
        start = 1L,
        stop = 2L
      ),
      bundesland = unname(
        bundesland_lookup[bundesland_id]
      ),
      grund_nicht_kartiert = dplyr::case_when(
        bundesland_id != "08" ~
          "Außerhalb Baden-Wuerttembergs",
        TRUE ~
          "GKZ nicht in der Gemeindedatei gefunden"
      )
    ) |>
    dplyr::select(
      gkz_join,
      bundesland_id,
      bundesland,
      grund_nicht_kartiert,
      n_tumors,
      n_near,
      n_not_near,
      share_near
    )
  
  # Nicht kartierbare Fälle zusammenfassen -----------------------------
  ## Kategorie außerhalb Baden-Württembergs bilden --------------------------
  
  outside_bw_summary <- gkz_join_check |>
    dplyr::filter(
      bundesland_id != "08"
    ) |>
    dplyr::summarise(
      area_group = "Außerhalb Baden-Wuerttembergs",
      n_tumors = sum(n_tumors),
      n_near = sum(n_near),
      n_not_near = sum(n_not_near),
      share_near = n_near / n_tumors
    )
  
  # BW-Gesamtwert berechnen -----------------------------
  ## Vergleichswert für den Farbbalken bilden --------------------------
  
  inside_bw_summary <- gkz_near_care_tab |>
    dplyr::filter(
      substr(gkz_join, 1L, 2L) == "08"
    ) |>
    dplyr::summarise(
      area_group = "Baden-Wuerttemberg",
      n_tumors = sum(n_tumors),
      n_near = sum(n_near),
      n_not_near = sum(n_not_near),
      share_near = n_near / n_tumors
    )
  
  area_near_care_summary <- dplyr::bind_rows(
    inside_bw_summary,
    outside_bw_summary
  )
  
  # Diagnostik ausgeben -----------------------------
  ## Nicht kartierbare GKZ und Gebietszusammenfassung anzeigen --------------------------
  
  print(
    gkz_join_check,
    n = Inf
  )
  
  print(
    area_near_care_summary
  )
  
  message(
    "Aggregierte Diagnose-GKZ: ",
    nrow(gkz_near_care_tab)
  )
  
  message(
    "Nicht kartierbare Diagnose-GKZ: ",
    nrow(gkz_join_check)
  )
  
  message(
    "Tumoren außerhalb Baden-Wuerttembergs: ",
    outside_bw_summary$n_tumors
  )
  
  # Farbbalken erstellen -----------------------------
  ## Baden-Württemberg mit außerhalb gelegenen Fällen vergleichen --------------------------
  
  p_near_care_area_bar <- area_near_care_summary |>
    dplyr::mutate(
      area_group = factor(
        area_group,
        levels = c(
          "Baden-Württemberg",
          "Außerhalb Baden-Württembergs"
        )
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = area_group,
        y = share_near,
        fill = share_near
      )
    ) +
    ggplot2::geom_col(
      width = 0.65
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(
          share_near,
          accuracy = 1
        )
      ),
      vjust = -0.4
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(
        accuracy = 1
      ),
      expand = ggplot2::expansion(
        mult = c(0, 0.08)
      )
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      limits = c(0, 1),
      guide = "none"
    ) +
    ggplot2::labs(
      title = paste0(
        "Anteil wohnortnaher Versorgung – ",
        config$analysis$active_output_group
      ),
      x = NULL,
      y = "Anteil wohnortnah"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 15,
        hjust = 1
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  print(
    p_near_care_area_bar
  )

}