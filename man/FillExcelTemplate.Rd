% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FillExcelTemplate.R
\name{FillExcelTemplate}
\alias{FillExcelTemplate}
\title{Funktion befuellt ein Excel Template.}
\usage{
FillExcelTemplate(
  tab1,
  tab2 = NULL,
  startingPoints,
  nrEmptyRows,
  inheritTemplateColNr = 1,
  customColNr = NULL,
  customCol = NULL,
  customCellList = NULL,
  f_in,
  sheet = 1,
  prefixTSN = "_",
  removeTemplateSheet = FALSE,
  removeAllTemplates = FALSE,
  interactive = TRUE,
  showFinalTab = FALSE,
  showSplitTab = FALSE
)
}
\arguments{
\item{tab1}{eine mit \code{MakeTable()} bzw.
\code{MakeAKETimeInstantsTable()} erzeugte Tabelle. Falls bei
\code{MakeTable()} limits angegeben und einzelne Zellen mit Klammern oder
Aehnlichem belegt wurden muss auch tab2 angegeben werden damit man die Werte
in den Zellen bekommt.}

\item{tab2}{NULL oder eine mit \code{MakeTable()} bzw.
\code{MakeAKETimeInstantsTable()} erzeugte Tabelle bei der limits NICHT
angegeben wurden. Diese Tabelle muss immer uebergeben werden wenn fuer die
Erstellung von tab1 limits beruecksichtigt wurden.}

\item{startingPoints}{numerischer Vektor: die Startzeilen der befuellten
Zeilen nach Leerzeilen im Original-Excel-File.}

\item{nrEmptyRows}{numerischer Vektor: Anzahl an Leerzeilen die vor
\code{startingPoints} kommen sollen; eigentlich immer 1 ausser vor grossen
Bloecken.}

\item{inheritTemplateColNr}{numerischer Vektor oder NULL: Spaltennummer/n der Tabellenspalten die vom Original-Excel-File uebernommen werden sollen.
Default ist die erste Spalte, also \code{inheritTemplateColNr=1}.}

\item{customColNr}{numerischer Wert: Spaltennummer der Tabellenspalte (derzeit nur EINE moeglich) die ueber
\code{customCol} individuell definiert werden soll und bei der die Leerzeilen
im Gegensatz zum Parameter \code{customCellList}
aus \code{startingPoints}  und \code{nrEmptyRows} uebernommen werden sollen.
Die Aufteilung von \code{tab1} auf die Spalten der Excel-Tabelle wird dann automatisch angepasst,
\code{tab1} bzw. \code{inheritTemplateColNr} wird also nicht ueberschrieben.}

\item{customCol}{character Vektor: Enthaelt die Eintraege der durch \code{customColNr}
definierten Tabellenspalte - falls diese nicht aus dem Original-Excel-File uebernommen werden sollen
und auch nicht durch \code{MakeTable()} generiert werden.
Im Gegensatz zum Parameter \code{customCellList} wird hier ein Character Vektor OHNE Missings uebergeben,
d.h. die ueber die Parameter \code{startingPoints} und \code{nrEmptyRows} definierten Leerzeilen
werden einfach uebernommen und muessen nicht extra beruecksichtigt werden.
Die Aufteilung von \code{tab1} auf die Spalten der Excel-Tabelle wird dann automatisch angepasst,
\code{tab1} bzw. \code{inheritTemplateColNr} wird also nicht ueberschrieben.}

\item{customCellList}{Listenobjekt: eine Liste (bzw. falls mehrere Zellen ueberschrieben werden sollen, eine Liste mit Sublisten) mit den Listenelementen
\code{row} (numeric), \code{col} (numeric) und \code{entry} (character).
Diese Listenelemente legen fest, welcher Zeile (row) und Spalte (col) die jeweilige Zelle entspricht
und was dort eingetragen werden soll (entry).
Um genau zu sein, legen \code{row} und \code{col} die jeweilige Start-Zeile und Start-Spalte in der Excel-Tabelle fest.
Somit koennen auch ganze Tabellenzeilen/-spalten auf einmal ueberschrieben werden,
dazu muss in \code{entry} lediglich ein Vektor der richtigen Laenge uebergeben werden. Siehe Examples.

Dieser Parameter ist anzuwenden falls eine (oder mehrere) Zellen individuell angepasst werden sollen,
z.B. um eine Fussnote einzufuegen oder um die Eintraege einer bestimmten Tabellenspalte mit bestimmten Inhalten zu ueberschreiben.

Zu beachten ist hier, dass die entsprechenden Zellen einfach nur ueberschrieben werden und die Aufteilung von \code{tab1}
auf die Spalten der Excel-Tabelle in keiner Weise angepasst wird.
Sollte man ZUSAETZLICH zu \code{tab1} (und evt. auch zusaetzlich zu \code{inheritTemplateColNr}) eine neue Spalte hinzufuegen wollen
so sind die Parameter \code{customColNr} bzw. \code{customCol} anzuwenden.}

\item{f_in}{File Name inklusive Pfad und File-Endungen des eingelesenen
Original-Excel-Files.}

\item{sheet}{Index oder Name des Excel-Sheets oder des zugehoerigen Template-Excel-Sheets.}

\item{prefixTSN}{Character: das Prefix des Namens des Template Sheets (siehe Details).
Default ist "_".}

\item{removeTemplateSheet}{TRUE/FALSE ob das Template-Excel-Sheet (mit dem Prefix \code{prefixTSN}) zum aktuell
bearbeiteten Excel-Sheet geloescht werden soll, also ob es nicht im ausgelesenen File enthalten sein soll.}

\item{removeAllTemplates}{TRUE/FALSE wie bei \code{removeTemplateSheet}, nur dass hier abgefragt wird,
ob ALLE Template-Excel-Sheets, also alle Sheets mit dem Prefix \code{prefixTSN},
geloescht werden sollen, also ob das ausgelesene File keine Templates mehr enthalten soll.}

\item{interactive}{Logical ob das Loeschen von Template-Sheets (\code{removeTemplateSheet},\code{removeAllTemplates})
erst manuell durch den Nutzer bestaetigt werden soll.}

\item{showFinalTab}{Logical: Falls TRUE, wird in R die Tabelle samt Leerzeilen ausgegeben wie
sie auch im ausgelesenen Excel-File landen wuerde. Ist dieser Parameter gesetzt, wird also kein Excel-File erstellt.}

\item{showSplitTab}{Logical: Falls TRUE, wird in R die durch \code{startingPoints} aufgesplittete Tabelle ausgegeben.
Ist dieser Parameter gesetzt, wird also kein Excel-File erstellt.}
}
\value{
Output ist ein Excel-File.
}
\description{
Funktion liest ein Excel-File ein, uebernimmt die Formatvorlage eines ausgewaehlten
Template-Excel-Sheets, befuellt die Kopie dieses Template-Excel-Sheets mit den Ergebnissen aus \link{MakeTable} und liest
das Ergebnis samt urspruenglichem Template-Excel-Sheet (default) und dem neu befuellten Excel-Sheet wieder als
Excel-File aus.
}
\details{
Ein Template-Excel-Sheet, das als Vorlage fuer das zu befuellende Excel-Sheet dient
und i.d.R. leer ist bis auf einige Formate, Header und Rownames, wird durch ein Prefix im Sheet-Namen
gekennzeichnet (\code{prefixTSN}). Der Sheet-Name ist bis auf das Prefix identisch
zum Sheet-Namen des zu befuellenden neuen Excel-Sheets. Sollte entweder Template oder
zu befuellendes neues Excel-Sheet noch nicht existieren, wird es automatisch
angelegt. Die Template-Sheets koennen bei Bedarf wieder einzeln (\code{removeTemplateSheet})
oder alle auf einmal (removeAllTemplates) geloescht werden. Sollten sowohl das Template
als auch das neue Sheet im File schon existieren ist es irrelevant ob bei \code{sheet}
das Template oder das neue Sheet angegeben wird.

Derzeit funktioniert diese Funktion nur fuer die Default-Werte von
\code{markLeft1}, \code{markRight1}, \code{markValue1}, \code{markLeft2},
\code{markRight2} und \code{markValue2} aus \code{MakeTable()} und
\code{MakeAKETimeInstantsTable()}.
}
\examples{
\dontrun{
###
Kommt wahrscheinlich ins mitgelieferte Bsp-File - samt Excel-Rohling.
###

### Beispiel einer customCellList:
# 1. die 1. Spalte der Tabelle soll ab der 1. Zeile der Tabelle die Eintraege 
# Category 1, Category 2 und Category 3 haben mit einer Leerzeile nach Category 1
# 2. wir wollen nur eine einzelne Zelle ansprechen und dort die Fussnote einfuegen 
# -> die 1. Spalte der Tabelle soll in Zeile 5 die Fussnote enthalten 
customCellList=list(
list(row=1,col=1,entry=c("Category 1", NA, "Category 2","Category 3")), 
list(row=5,col=1,entry="FussnoteBlaBlaText")
)
# bzw dasselbe in anderer Schreibweise:
customCellList <- list()
customCellList[[length(customCellList)+1]] <- 
list(row=1,col=1,entry=c("Category 1", NA, "Category 2","Category 3"))
customCellList[[length(customCellList)+1]] <- 
list(row=5,col=1,entry="FussnoteBlaBlaText")
}

}
\seealso{
\code{\link{MakeTable},\link{MakeQT},\link{ImportData},\link{IndivImportData},\link{ImportDataListQT}}
}
