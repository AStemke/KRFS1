# KRFS1 R-Skripte

Diese Skripte folgen der README-Struktur:

- `data/Dummy/raw`: Dummydaten als `Dummy_Datensatz_DATUM.xlsx` und `Dummy_Datensatz_DATUM_Legende.xlsx`
- `data/Productive/raw`: Produktivdaten als `Productive_Datensatz_DATUM.xlsx` und `Productive_Datensatz_DATUM_Legende.xlsx`
- `R`: R-Skripte mit Namensschema `XX_YY_ZZ_Namensgebend.R`
- `output`: Von R erzeugte Tabellen, Grafiken, RDS-Dateien und Logs

Startpunkt ist `00-Main_Setup.R` im Projektroot.

## Output-Kategorien und Datenschutz

Die zentrale Regel lautet: Jede ausgegebene Zellfallzahl muss entweder `0` oder `>= config$thresholds$n_min` sein. Kritisch ist `0 < n < n_min`. Je Kategorie wird eine passende Fallzahlspalte geprueft:

- `aggregate_table`: normale aggregierte Tabellen
- `aggregate_figure`: Grafiken auf Basis aggregierter Tabellen
- `descriptive_numeric`: numerische Kennzahlen, z.B. Distanzmedian je Gruppe
- `crosstab`: Kreuztabellen; jede beobachtete Zelle wird geprueft
- `map_area`: Karten-/Gebietsauswertungen nach GKZ/PLZ/Region
- `statistical_test`: Testoutputs nur, wenn die zugrunde liegenden Zellen safe sind
- `model_result`: Modelloutputs nur, wenn Modellframe, Outcome-Zellen und Faktorlevel safe sind
- `technical_overview`: rein technische Checks ohne sensible Zellfallzahlen

Unterdrueckte Outputs erscheinen im jeweiligen Excel-Sheet `suppressed_outputs` und in `output/logfiles`.

## Geodaten

Geodaten sind optional vorbereitet. Erwartet werden z.B. Gemeindepolygone als `data/geodata/gemeinden.gpkg`. Pfad, Layer und Join-Key stehen in `R/00_Setup/00_00-Config.R` unter `config$geodata`.

Wenn keine Geodaten vorhanden sind, werden die Karten uebersprungen, aber die aggregierten Gebietstabellen weiterhin erzeugt und datenschutzgeprueft.
