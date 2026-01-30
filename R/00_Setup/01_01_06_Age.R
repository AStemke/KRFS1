# 01_01_06_Age
# Zweck: Altersverteilung und Altersklassen

# Tabelle: Alter Summary---------------------------
age_num <- suppressWarnings(as.numeric(dat$Alter))

age_summary <- dplyr::tibble(
  n_total = length(age_num),
  n_na    = sum(is.na(age_num)),
  min     = min(age_num, na.rm = TRUE),
  median  = stats::median(age_num, na.rm = TRUE),
  mean    = mean(age_num, na.rm = TRUE),
  max     = max(age_num, na.rm = TRUE)
)

ctx <<- tc_write_table(
  tab = age_summary,
  table_id = "01_06_age_summary",
  ctx = ctx,
  threshold = FALSE
)

## Tabelle: Altersklassen------------------------------------
age_group <- cut(
  age_num,
  breaks = config$explore$age_breaks,
  labels = config$explore$age_labels,
  right = FALSE,
  include.lowest = TRUE
)

age_group_tab <- dplyr::tibble(age_group = age_group) %>%
  dplyr::count(age_group, name = "n") %>%
  dplyr::mutate(pct = n / sum(n))

ctx <<- tc_write_table(
  tab = age_group_tab,
  table_id = "01_06_age_groups",
  ctx = ctx,
  threshold = TRUE,
  threshold_label_cols = c("age_group")
)

## Grafik: Altersklassen-------------------------------------
p_age <- ggplot2::ggplot(
  age_group_tab,
  ggplot2::aes(x = age_group, y = n)
) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Altersklassen", x = NULL, y = "Anzahl")

ctx <<- tc_write_figure(
  plot = p_age,
  fig_id = "01_06_age",
  filename = "age_groups_bar",
  ctx = ctx,
  threshold = TRUE,
  threshold_tab = age_group_tab,
  threshold_label_cols = c("age_group")
)
