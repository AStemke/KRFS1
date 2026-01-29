# 00_02-Data_Setup

source_setup_helpers(
  current_wd = current_wd,
  setup_subdir = file.path("R", "00_Setup"),
  prefix = "00_02_",
  verbose = FALSE,
  local = FALSE
)

ads <- load_ads(current_wd, productive = FALSE)

# Distanz numeric?
str(ads$data$Distanz_km)
summary(ads$data$Distanz_km)

# Beispiel-Faktor inkl. Levels aus Legende
str(ads$data$STRGESCHLECHT)
levels(ads$data$STRGESCHLECHT)
table(ads$data$STRGESCHLECHT, useNA = "ifany")
