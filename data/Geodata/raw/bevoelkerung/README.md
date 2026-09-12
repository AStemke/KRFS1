# Bevoelkerungsdaten fuer die regionale Referenzkarte

In diesem Ordner liegt die amtliche Gemeindedatei fuer die Bevoelkerungs-Heatmap.
Die Datei selbst wird nicht automatisch aus dem Internet geladen.

## Empfohlener Stand

Fuer den Projektzeitraum wird der Bevoelkerungsstand zum 31.12.2023 empfohlen.
Die Karte wird nur fuer `Selektiv_Gesamt` erzeugt und verwendet dieselbe regionale
Geometrie sowie dieselben Zentrenmarker wie die regionale Near-Care-Heatmap.

## Empfohlene Quelle

Bevorzugt wird ein Export aus der Regionaldatenbank Deutschland, Tabelle
`12411-01-01-5` (Bevoelkerung nach Geschlecht, Stichtag 31.12., regionale Tiefe:
Gemeinden). Fuer den Export Baden-Wuerttemberg, Stichtag 31.12.2023 und
Geschlecht Insgesamt auswaehlen.

Alternativ kann die Gemeindedatei des Statistischen Landesamts Baden-Wuerttemberg
zu den Gemeindegrößenklassen mit Bevoelkerungsstand 31.12.2023 verwendet werden.
Das Skript erkennt insbesondere die Datei `GSGG20231201-20231208_G09.xlsx`.

## Dateinamen

Bevorzugte Dateinamen sind:

- `bevoelkerung_gemeinden_2023.xlsx`
- `bevoelkerung_gemeinden_2023.csv`
- `bevoelkerung_gemeinden.xlsx`
- `bevoelkerung_gemeinden.csv`
- `GSGG20231201-20231208_G09.xlsx`

Wenn genau eine XLSX-, XLS- oder CSV-Datei im Ordner liegt, wird auch ein anderer
Dateiname akzeptiert.

## Benoetigte Inhalte

Die Datei muss eine Gemeinde-/AGS-/GKZ-Spalte und eine Spalte mit der gesamten
Bevoelkerungszahl enthalten. Sechsstellige baden-wuerttembergische
Gemeindeschluessel wie `111000` bzw. `GS111000` werden automatisch auf die
achtstellige GKZ `08111000` erweitert.
