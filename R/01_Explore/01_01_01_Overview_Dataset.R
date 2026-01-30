# 01_01_01_Overview_Dataset
# Zweck: Grundlegender Überblick über Datensatzgröße und Zeitraum

# Tabelle: Dataset-Overview---------------------------
.safe_min <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA else min(x) }
.safe_max <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA else max(x) }

created_at <- if (!is.null(ads_list$meta$created_at)) ads_list$meta$created_at else NA
source_data_file <- if (!is.null(ads_list$meta$source_data_file)) ads_list$meta$source_data_file else NA

overview_dataset <- dplyr::tibble(
  n_rows          = nrow(dat),
  n_cols          = ncol(dat),
  n_tumors        = dplyr::n_distinct(dat$LNGTUMORID),
  n_patients      = dplyr::n_distinct(dat$LNGREGISTERNR),
  first_diag      = .safe_min(dat$DIAGNOSEDATUM),
  last_diag       = .safe_max(dat$DIAGNOSEDATUM),
  created_at      = created_at,
  source_data_file = source_data_file
)

ctx <<- tc_write_table(
  tab = overview_dataset,
  table_id = "01_01_overview_dataset",
  ctx = ctx,
  threshold = FALSE
)
