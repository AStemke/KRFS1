# 00_02-Data_Setup

# Derzeit keine weiteren Skripte
# source_setup_helpers(
#   current_wd = current_wd,
#   setup_subdir = file.path("R", "00_Setup"),
#   prefix = "00_02_",
#   verbose = FALSE,
#   local = FALSE
# )

#Liest ADS ein, falls vorhangen, andernfalls erstellt es aus den Excel files ein ads
ads_list <- load_ads(current_wd, productive = FALSE)

# # Distanz numeric?
# str(ads_list$data$Distanz_km)
# summary(ads_list$data$Distanz_km)
# 
# # Beispiel-Faktor inkl. Levels aus Legende
# str(ads_list$data$STRGESCHLECHT)
# levels(ads_list$data$STRGESCHLECHT)
# table(ads_list$data$STRGESCHLECHT, useNA = "ifany")


