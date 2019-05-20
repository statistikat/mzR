mount_mz_ergebnisse <- function() {
  b_mz <- mountSTAT::mountWinShare(
    server = "DatenB", share = "B_MZ", mountpunkt = "mz", 
    verbose = FALSE
  )
  file.path(b_mz, "AKE Neu ab 2004", "06 Ergebnisse")
}

#' Sehr spezifische Funktion die MZ-Daten einliest um damit in weiterer Folge Tabellen im
#' MZ-Quartalsbericht-Format zu erstellen (hausintern).
#' 
#' Funktion liest MZ-Daten (dg7) automatisch aus dem STAT-Filemanagement ein und fuehrt diese Daten
#' mit dem derzeit fuer die Quartalsberichtsproduktion verwendeten SPSS-File zusammen (siehe Details).
#' 
#' Anzugeben ist bei dieser Funktion der Referenzzeitpunkt \code{timeInstant} des MZ-Quartalsberichts.
#' Die MZ-Daten (dg7) werden (derzeit) defaultmaessig (\code{ImportAndMerge=TRUE})  
#' ueber \code{mergeBy = c("asbper","ajahr","amonat")} mit \emph{Daten_ab2004_QuartPub.sav} 
#' gemerged. Dieses File wird quartalsweise vom Fachbereich erzeugt und immer im selben Ordner abgelegt.
#' Sollte man mit einem anderen File mergen wollen, so kann man die entsprechenden File-Pfade in 
#' \code{curr_inFile} und \code{prev_inFile} spezifizieren aber auch \code{mergeBy} anpassen, analog zu \link{ImportAndMerge}.
#' Des Weiteren koennen die einzulesenden MZ-Daten mit den Funktionsparametern
#' \code{nbw}, \code{whichVar} und \code{weightDecimals} angepasst werden, siehe \link{ImportData}. 
#' Dadurch kann man z.B. vermeiden, dass Variablen die sowohl in den MZ-Daten als auch in 
#' \emph{Daten_ab2004_QuartPub.sav} vorkommen doppelt eingelesen werden.
#' 

#' @param timeInstant numerischer Vektor mit 2 Elementen: c(jahr, quartal).
#' Hier gibt man den Zeitpunkt an auf den sich alle Ergebnisse im weitesten
#' Sinn beziehen sollen, also i.d.R. das aktuellste Quartal.
#' @param nbw numerischer Wert: Anzahl an Bootstrap-Gewichten die eingelesen
#' werden soll (z.B. um Rechenzeit beim Aufsetzen der Tabellen zu verkuerzen).
#' @param whichVar Character (vector) oder NULL. Falls ungleich NULL, Character Vektor mit Variable(n) aus
#' dem dg7-Mikrozensus-File die im Output-File enthalten sein sollen. Die
#' uebrigen Variablen werden weggelassen. Default ist NULL, dabei werden alle
#' Variablen behalten.
#' @param weightDecimals Numerischer Wert oder NULL. Anzahl der Nachkommastellen der Stichprobengewichte, 
#' gerundet nach SPSS RND Logik (0.5 bwz. -0.5 wird dabei immer "weg von 0" gerundet). 
#' Falls NULL, werden die Gewichte nicht gerundet.
#' @param ImportAndMerge TRUE/FALSE ob die Funktion \link{ImportAndMerge} angewendet werden soll. 
#' Bei der Defaulteinstellung \code{ImportAndMerge=TRUE} und \code{curr_inFile=NULL} wird \emph{Daten_ab2004_QuartPub.sav}
#' zu den MZ-Daten gemerged. 
#' @param curr_inFile Pfad der Datei die eingelesen und zu den MZ-Daten gemerged werden soll (bezogen auf
#' den aktuelleren der beiden Zeitpunkte falls prev_inFile ungleich NULL).
#' Eingelesen werden koennen Files vom Typ .sav, .csv und .csv.gz.
#' @param prev_inFile Falls ungleich NULL, Pfad der Datei die eingelesen und zu den MZ-Daten gemerged werden
#' soll (bezogen auf den weniger aktuellen Zeitpunkt). Eingelesen werden koennen
#' Files vom Typ .sav, .csv und .csv.gz.
#' @param mergeBy Character Vektor mit Variable(n) nach denen gemerged werden
#' soll (default=c("asbper","ajahr","amonat")).
#' @inheritParams ImportData
#' @param mz_ergebnisse Pfad zu dem `06 Ergebnisse` Ordner in der STAT
#'   Infrastruktur. Standardmäßig wird dieser mit `mountSTAT` generiert.
#' 
#' @return Output ist eine Liste deren Elemente jeweils MZ-Daten enthalten 
#' die die selbe Grundstruktur haben wie der Output aus der Funktion \link{ImportData}.

#' @seealso
#' \code{\link{MakeQT},\link{MakeTable},\link{FillExcelTemplate},\link{ImportData},\link{ImportAndMerge}}
#' @export
#' @examples
#' \dontrun{
#' ## Lesen Daten fuer den AKE-QT-Referenzzeitpunkt 2014q4 ein.
#' # Fuer Testzwecke (um Rechenzeit zu sparen) schraenken wir die 
#' # Anzahl der Bootstrapgewichte ein auf 5.
#' # Ausserdem wollen wir aus den Original-MZ-Daten (dg7) nur die 
#' # Variable rbpkin behalten.
#' 
#' datalist <- ImportDataListQT(timeInstant=c(2014,4), nbw=5, whichVar=c("rbpkin"))
#' 
#' }
#' 

ImportDataListQT <- function(
  timeInstant, nbw = NULL, whichVar = NULL, weightDecimals = 2,
  ImportAndMerge = TRUE, curr_inFile = NULL, prev_inFile = NULL, 
  mergeBy = c("asbper", "ajahr", "amonat"), mz_intern = mount_mz_intern(),
  mz_ergebnisse = mount_mz_ergebnisse()
) {
  if(ImportAndMerge && is.null(curr_inFile)){
    if(file.exists(paste0(mz_ergebnisse, "/Quartalsberichte fertig/Quartalsbericht ", timeInstant[1],
                          " Q", timeInstant[2]))){
      qt_spss_path_curr <- qt_spss_path_prev <- paste0(
        mz_ergebnisse, "/Quartalsberichte fertig/", "Quartalsbericht ",
        timeInstant[1], " Q", timeInstant[2], "/Daten/Daten_ab2004_QuartPub.sav")
      warning("\n\nACHTUNG: timeInstant=c(",timeInstant[1],",",timeInstant[2],") entspricht nicht dem aktuellsten",
              "Referenzzeitpunkt fuer MZ-Quartalstabellen!\n", "Es wird also \n'",qt_spss_path_curr,
              "' \n ueber ImportAndMerge eingelesen statt \n'/mnt/mz/AKE Neu ab 2004/06 Ergebnisse/Quartalsberichte",
              "Produktion/Daten/Daten_ab2004_QuartPub.sav'\n\n")
    }else{
      qt_spss_path_curr <- qt_spss_path_prev <- paste0(
        mz_ergebnisse, "/Quartalsberichte Produktion/",
        "Daten/Datensatz_QuartPub.sav")
    }
  }
  
  datalist <- list()

  if(ImportAndMerge && !is.null(curr_inFile)){
    qt_spss_path_curr <- curr_inFile
    if(is.null(prev_inFile)){
      stop("prev_inFile muss angegeben werden da bei QT Veraenderungen berechnet werden!")
    }
    qt_spss_path_prev <- prev_inFile
  }
  
  
  
  # Sichergehen, dass mergeBy-Variable(n) bei eingeschraenktem Datensatz dabei sind  
  if(!is.null(whichVar)){
    if(!all(mergeBy%in%whichVar)){
      whichVar <- c(mergeBy[which(!mergeBy%in%whichVar)],whichVar)
    }
  }
  
  ende <- timeInstant  
  
  tInterval <- as.numeric(time(ts(start=start(lag(ts(end=ende,frequency=4),3)),end=ende,frequency=4)))
  tInterval <- date_decimal(tInterval)
  jahr_seq <- year(tInterval)
  quartal_seq <- quarter(tInterval)
  
  ## 'POSIXct, POSIXt' object
  # vjq <- lag(ts(end=ende,frequency=4),4)
  # vjq <- as.numeric(time(vjq))
  # vjq <- date_decimal(vjq)
  # vjq_jahr <- year(vjq)
  # vjq_quartal <- quarter(vjq)

  vvjq <- lag(ts(end=ende,frequency=4),8)
  vvjq <- as.numeric(time(vvjq))
  vvjq <- date_decimal(vvjq)
  vvjq_jahr <- year(vvjq)
  vvjq_quartal <- quarter(vvjq)
  
  timeInstants_jahr <- c(vvjq_jahr,jahr_seq)
  timeInstants_quartal <- c(vvjq_quartal,quartal_seq)
  
  
  for(i in 1:length(timeInstants_jahr)){
    jahr <- timeInstants_jahr[i]
    quartal <- timeInstants_quartal[i]
    
    if(jahr==ende[1]&&quartal==ende[2]){
      ### Aktuelles Quartal plus Vorquartal
      dat <- ImportData(
        year = jahr, quarter = quartal, comp_diff_lag = 1, nbw = nbw, 
        whichVar = whichVar, weightDecimals = weightDecimals, 
        mz_intern = mz_intern)
      if(ImportAndMerge){
        dat <- ImportAndMerge(dat,curr_inFile=qt_spss_path_curr, prev_inFile=qt_spss_path_prev,mergeBy=c("asbper","ajahr","amonat"))
      }
      datalist[[length(datalist)+1]] <- dat 
      names(datalist)[length(datalist)] <- paste0("dat_",jahr,"q",quartal,"vq")
      rm(dat);gc()
      ### Vorjahresquartal
      dat <- ImportData(
        year = jahr, quarter = quartal, comp_diff_lag = 4, nbw = nbw,
        whichVar = whichVar,weightDecimals = weightDecimals, 
        mz_intern = mz_intern)
      if(ImportAndMerge){
        dat <- ImportAndMerge(dat,curr_inFile=qt_spss_path_curr, prev_inFile=qt_spss_path_prev,mergeBy=c("asbper","ajahr","amonat"))
      }
      datalist[[length(datalist)+1]] <- dat 
      names(datalist)[length(datalist)] <- paste0("dat_",jahr,"q",quartal,"vjq")
      rm(dat);gc()
    }else if(!identical(c(jahr,quartal),c(jahr_seq[length(jahr_seq)-1] ,quartal_seq[length(quartal_seq)-1]))){
      ### Rest
      dat <- ImportData(
        year = jahr, quarter = quartal, nbw = nbw, whichVar = whichVar,
        weightDecimals = weightDecimals, mz_intern = mz_intern)
      if(ImportAndMerge){
        dat <- ImportAndMerge(dat,curr_inFile=qt_spss_path_curr,mergeBy=c("asbper","ajahr","amonat"))
      }
      datalist[[length(datalist)+1]] <- dat 
      names(datalist)[length(datalist)] <- paste0("dat_",jahr,"q",quartal)
      rm(dat);gc()
    }
  }
  
  return(datalist)
}
