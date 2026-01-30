# KRFS1
Wohnohrtnahe Versorgung in der Metropolregion Stuttgart

# Version
Die Idee der Versionierung:
X-Y-Z

X ist ein Major Release, und ist eine Version bei der weitreichende 
Funktionialitäten verändert worden sind, sobald X=1 geteilt wurde.

Y enthält grundlegende Veränderungen in znetralen Funktionen, oder Überarbeitungen
nach Rücksprache. Oft verwendet bei einem Merge auf Main.

Z ist für jedwede nennenswerte Änderungen im Entwicklungsprozess, spätestens sobald eine
neue Version geteilt wurde, sei es auch nur ein neuer Kommentar. 
Häufig verwendet, für bspw. Veränderungen auf einem Branch.

## 0-0-1 First release
Diese Version hat inhaltlich noch wenig Relevanz, sie dient eher der gemeinsamen 
Kommunikation, wie das gemeinsame Arbeiten mit dem Code und an den Daten stattfindet.

Bekannte Bugs:
Threshold Funktion (00_01_01_Helper_Threshold_Log) funktioniert noch nicht 
wie intendiert, hier besteht auch noch Klärungsbedarf.

## History
### Keine

# Projektverzeichnis
Die Ordnerstrukur ist wie folgt aufgebaut:

KRFS1
-data
-docs
-misc
-output
-R
-text

Wobei KRFS1 für Krebsregister Fragestellung 1 darstellt und unsere interne 
Bezeichnung für dieses Projekt ist. 


## data
Der Ordner data enthält sämtliche Dateien, welche zur Auswertung notwendig sind. 
Der Unterordner Dummy enthält die Dummydaten, welche vom KR zur Verfügung 
gestellt wurden.
Hier bitte auch die finalen Daten zur Auswertung ablegen. 
Unterteilt wird hier in

data

-Dummy
--ads
--raw

-Productive
--ads
--raw

Wobei raw die Dateien enthalten, wie wir sie bekommen haben und ads steht für 
analysis data set, mit diesem werden dann die Auswertungen durchgeführt, 
um Codierungsprobleme und anderes auszuschließen, sollte es sich dabei um ein
RData Objekt handeln. 

Damit das Prorgammreibungslos läuft, sollte das Bennennungsschema beibehalten 
werden und diese in raw abgelegt werden, außerdem darf in dem Ordner nur jeweils
eine xlsx dem Muster entsprechen
Dummy_Datensatz_DATUM.xlsx
Dummy_Datensatz_DATUM_Legende.xlsx
in Dummy/raw und
Productive_Datensatz_DATUM.xlsx
Productive_Datensatz_DATUM_Legende.xlsx
in Productive/raw

## docs
Dieser Ordner beinhaltet Dokumente wir Präsentationen etc

# misc
Ist eine Sonstige Kategorie, miscellaneous.
Ablagen hier sind zu vermeiden, aber gelegentlich doch notwendig.

# output
Dieser Ordner enthält sämtlichen von R generierten Output.
Diese werden an den OSP zurückgespiegelt und sollten bezüglich Ihres Outputs 
kontrolliert werden.

# R
Sämtlicher R-Code ist hier abgelegt.
XX_YY_ZZ_Namensgebend.R ist die allgemeine Struktur der Bennenung, 
wobei hier von 00 zu 99 aufsteigend gezählt wird.
Ein Styleguide wird versucht einzuhalten.
Es gibt auch Dateien der Form XX_YY-Namensgebend.R, diese sourcen dann alle 
notwendigen Unterfunktionen.

# text
Sämtliche reine Textnotizen (mit Ausnahme von gitignore, README und ähnlichen), 
Literatur, Quellen dem Projekt zugehörig werden hier hinterlegt.


