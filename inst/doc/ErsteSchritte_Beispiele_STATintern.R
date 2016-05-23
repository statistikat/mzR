# Paket installieren
#devtools::install_github("statistikat/mzR")
install.packages("...Pfad.../mzR_1.0.0.zip", repos = NULL,type="binary")

# Paket laden
require(mzR)

# Paket laden
require(mzR)

# Hilfe zu mzR-Paket (mit Ueberblick ueber alle Funktionen)
help(pa=mzR)
# Hilfe zu Funktion GroupSize() aufrufen
?GroupSize
# Hilfe zur R-Syntax
?Syntax


# 1_Daten einlesen - Beispiel 1:
# Quartal und zugehoeriges Vorjahrsquartal einlesen
dat <- ImportData(year=2014,quarter=4, comp_diff_lag=4)


# Absolutwerte der Levels von xerwstat
GroupSize(dat,each="xerwstat")
# Labels zu xerwstat
GetLabels(dat,"xerwstat")

# Arbeitslosenzahlen: Absolutwerte und Veraenderung
GroupSize(dat,TFstring="xerwstat==2 & balt>=15 & balt<=74")
# Arbeitslosenquoten: Prozentwerte und Veraenderung 
GroupRate(dat,TFstring="xerwstat==2 & balt>=15 & balt<=74", TFstring2="xerwstat%in%c(1,2) & balt>=15 & balt<=74")
# Arbeitslosenzahlen: Absolutwerte und Veraenderung fuer jedes Bundesland
GroupSize(dat,TFstring="xerwstat==2 & balt>=15 & balt<=74", each="xnuts2")
# Lables zur Variable xnuts2
GetLabels(dat,"xnuts2")

# Absolutwerte und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
GroupSize(dat,TFstring="balt>=15 & balt<=74", each="xerwstat")
# Prozentwerte  und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
# (bezogen auf Gesamtbevoelkerung, deshalb TFstring2=NULL und byeach=FALSE)
GroupRate(dat,TFstring="balt>=15 & balt<=74", each="xerwstat", byeach=FALSE)
# Prozentwerte und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
# (bezogen auf die Auspraegungen von xerwstat, deshalb byeach=TRUE)
GroupRate(dat,TFstring="balt>=15 & balt<=74", each="xerwstat", byeach=TRUE)

# Geleistete Arbeitsstunden: Absolutwerte
Total(dat,TFstring="xerwstat==1 & balt>=15 & balt<=74", var="estund*13 + dtstd*13")
# Geleistete Arbeitsstunden: Absolutwerte in Millionen
Total(dat,TFstring="xerwstat==1 & balt>=15 & balt<=74", var="(estund + dtstd)*13/10^6")
# Tatsaechlich geleistete Arbeitsstunden (Arbeitsvolumen) der Bevoelkerung in Privathaushalten
# ohne Praesenz- und Zivildiener, ohne Personen in Elternkarenz (mit aufrechtem Dienstverhaeltnis)
# in Millionen (wie in Quartalsberichtstabelle C3)
Total(dat,TFstring="xerwstat==1 & xpkarenz!=1", var="(estund + dtstd)*13/10^6")

#Durchschnittlich geleistete Arbeitsstunden
Mean(dat,TFstring="xerwstat==1 & balt>=15 & balt<=74",var="estund+dtstd")

#Durchschnittlich geleistete Arbeitsstunden nach Bundesland
Mean(dat,TFstring="xerwstat==1 & balt>=15 & balt<=74",var="estund+dtstd",each="xnuts2")

# Neue Variable in den Daten erzeugen, z.B. eine neue Variable "Normalarbeitsstunden Haupt- 
# und Nebentätigkeit zusammen":
stdges_fun <- function(dstd, estd){
  x <- vector()
  x[dstd==999] <- 999
  x[dstd==-3] <- -3
  x[dstd!=999] <- dstd[dstd!=999] 
  x[estd!=-3 & dstd!=-3 & dstd!=999] <- 
    dstd[estd!=-3 & dstd!=-3 & dstd!=999]+estd[estd!=-3 & dstd!=-3 & dstd!=999]
  return(x)
}
dat <- AddVariable(x=dat, functionName=stdges_fun, newVar="stdges")


# Daten einlesen - Beispiel 2:
# Haushalte: Quartal und Vorquartal einlesen.
dathh <- ImportData(year=2014,quarter=4, comp_diff_lag=1, hh=TRUE)

# Absolutwerte: Anzahl der Hauptmietwohnungen ohne gueltiger Kostenangabe.
GroupSize(dathh,TFstring="wrecht==3")
GroupSize(dathh,TFstring="wrecht2%in%c(1:3)")

# Lables zur Variable wrecht
GetLabels(dathh,"wrecht")

# Ergebnisse als .csv ins Work Directory rausschreiben (getwd() zeigt das Work Directory an)
willRaus <- GroupSize(dathh,TFstring="wrecht2%in%c(1:3)")
export(willRaus)
# bzw
export(GroupSize(dathh,TFstring="wrecht2%in%c(1:3)"))
# Ergebnisse unter anderem Namen als .csv ins D: Verzeichnis speichern
export(willRaus, outFilePath="D:", outFileName="Bin jetzt draussen")


