# Paket installieren
#devtools::install_github("statistikat/mzR")
install.packages("...Pfad.../mzR_1.0.0.zip", repos = NULL,type="binary")

# Paket laden
require(mzR)


# Hilfe zu mzR-Paket (mit Ueberblick ueber alle Funktionen)
help(pa=mzR)
# Hilfe zu Funktion GroupSize() aufrufen
?GroupSize
# Hilfe zur R-Syntax
?Syntax


# Testdaten laden 
data(mzTestData)
# Die Funktion zum Einlesen der Daten fuer externe DatennutzerInnen der 
# Mikrozensus-Arbeitskraefteerhebung heisst IndivImportData(). 
# Hilfe zu IndivImportData aufrufen:
?IndivImportData


# Absolutwerte der Levels von xerwstat
GroupSize(mzTestData,each="xerwstat")
# Labels zu xerwstat
GetLabels(mzTestData,"xerwstat")

# Arbeitslosenzahlen: Absolutwerte und Veraenderung
GroupSize(mzTestData,TFstring="xerwstat==2 & balt>=15 & balt<=74")
# Arbeitslosenquoten: Prozentwerte und Veraenderung 
GroupRate(mzTestData,TFstring="xerwstat==2 & balt>=15 & balt<=74", TFstring2="xerwstat%in%c(1,2) & balt>=15 & balt<=74")

# Absolutwerte und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
GroupSize(mzTestData,TFstring="balt>=15 & balt<=74", each="xerwstat")
# Prozentwerte  und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
# (bezogen auf Gesamtbevoelkerung, deshalb TFstring2=NULL und byeach=FALSE)
GroupRate(mzTestData,TFstring="balt>=15 & balt<=74", each="xerwstat", byeach=FALSE)
# Prozentwerte und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
# (bezogen auf die Auspraegungen von xerwstat, deshalb byeach=TRUE)
GroupRate(mzTestData,TFstring="balt>=15 & balt<=74", each="xerwstat", byeach=TRUE)

# Geleistete Arbeitsstunden: Absolutwerte
Total(mzTestData,TFstring="xerwstat==1 & balt>=15 & balt<=74", var="estund*13 + dtstd*13")
# Geleistete Arbeitsstunden: Absolutwerte in Millionen
Total(mzTestData,TFstring="xerwstat==1 & balt>=15 & balt<=74", var="(estund + dtstd)*13/10^6")

#Durchschnittlich geleistete Arbeitsstunden
Mean(mzTestData,TFstring="xerwstat==1 & balt>=15 & balt<=74",var="estund+dtstd")
#Durchschnittlich geleistete Arbeitsstunden nach Bundesland
Mean(mzTestData,TFstring="xerwstat==1 & balt>=15 & balt<=74",var="estund+dtstd",each="xnuts2")

# Neue Variable in den Daten erzeugen, z.B. eine umkodierte Erwerbsstatus-Variable:
# Die Variable xerw soll dabei fuer die 15-24-Jaehrigen dieselben Auspraegungen enthalten 
# wie die Variable xerwstat und -99 sonst. 
xerw_fun <- function(balt, xerwstat) {
  x <- ifelse(balt>=15 & balt <=24, xerwstat, -99)
  return(x)
}
mzTestData <- AddVariable(x=mzTestData, functionName=xerw_fun, newVar="xerw")

# Ergebnisse als .csv ins Work Directory rausschreiben (getwd() zeigt das Work Directory an)
willRaus <- GroupSize(mzTestData,TFstring="xerwstat==2&balt>=15&balt<=74")
export(willRaus)
# bzw
export(GroupSize(mzTestData,each="bsex"))
# Ergebnisse unter anderem Namen als .csv ins D: Verzeichnis speichern
export(willRaus, outFilePath="D:", outFileName="Bin jetzt draussen")


