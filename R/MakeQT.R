#' Sehr spezifische Funktion zur Erstellung von Tabellen im
#' MZ-Quartalsbericht-Format.
#' 
#' Funktion erstellt Tabellen in einem Format das im
#' Mikrozensus-Quartalsbericht ueblich ist, also mit Ergebnissen fuer verschiedene Zeitpunkte. 
#' Die zugehoerigen MZ-Daten muessen dabei vorher mit der Funktion \link{ImportDataListQT} 
#' eingelesen werden.
#' 
#' Die zur Erstellung der Quartalsberichts-Tabellen benoetigten Daten muessen zuvor mit
#' der Funktion \link{ImportDataListQT} eingelesen werden.
#' 
#' \strong{col}
#' 
#' \code{col} ist eine aus Sublisten bestehende Liste. Jede Subliste steht fuer
#' eine eigene Spalte der zu erstellenden Tabelle in Matrix-Form und gibt an,
#' was fuer die entsprechende Spalte berechnet werden soll. Jede Subliste kann
#' die Argumente \code{fun}, \code{TFstring}, \code{TFstring2}, \code{digits},
#' \code{var} und \code{scaleF} enthalten. \code{fun} muss dabei gesetzt werden
#' und zwar auf eine der vier Funktionen \code{GroupSize}, \code{GroupRate},
#' \code{Mean} und \code{Total} aus dem mzR-Paket, die restlichen Parameter
#' sind optional. \code{TFstring}, \code{TFstring2}, \code{digits} und
#' \code{var} sind einfach die Parameter aus eben genannten Funktionen,
#' \code{scaleF} ist der Skalierungsfaktor der auf die jeweilige Spalte
#' angewendet werden soll, also z.B. \code{scaleF="/1000"} gibt an, dass die
#' Ergebnisse der entsprechenden Spalte durch 1000 dividiert werden sollen, er
#' ist als character zu uebergeben.
#' 
#' \strong{row}
#' 
#' \code{row} entspricht in dieser Funktion im Prinzip dem 
#' \code{block}-Parameter von \link{MakeTable}.
#' 
#' \code{row} ist eine 'list', 'named list' oder 'partly named list' die
#' Sublisten enthalten kann aber nicht muss. Sublisten werden eigentlich nur
#' uebergeben wenn einem Element aus \code{row} mehr als ein Parameter
#' zugewiesen werden soll. Zusaetzlich zu den moeglichen Argumenten fuer
#' \code{col} (ausser \code{digits}) kann hier auch noch \code{each} gesetzt
#' werden (auch ein Parameter aus den mzR-Schaetz-und
#' Fehlerrechnungsfunktionen), \code{fun} ist hier optional. Wird der Parameter
#' \code{scaleF} bei \code{row} gesetzt, so hat er den Vorzug vor \code{scaleF}
#' bei \code{col} - sollte er dort gesetzt worden sein (hier nicht vergessen,
#' falls in \code{row} Raten berechnet werden sollen \code{scaleF="*1"}
#' setzen). Dasselbe gilt weitgehend auch fuer \code{fun}, solange es Sinn
#' macht - hier kann es zu Faellen kommen wo kein Ergebnis ausgegeben werden
#' kann. 
#' 
#' Info: \code{returnCommands} wie in \code{MakeTable()} ist hier (noch?) nicht
#' implementiert.
#' 
#' @param datalist Output-Objekt der Funktion ImportDataListQT()
#' @param col Listenobjekt um Spalten zu definieren, siehe Details.
#' @param row Listenobjekt oder NULL um Zeilen zu definieren, siehe Details.
#' @param timeInstant numerischer Vektor mit 2 Elementen: c(jahr, quartal).
#' Hier gibt man den Zeitpunkt an auf den sich alle Ergebnisse im weitesten
#' Sinn beziehen sollen, also i.d.R. das aktuellste Quartal.
#' @param lim1 numerischer Wert: falls \code{lim1}>\code{error}, wird der
#' entsprechende Wert von \code{estimator} in der Tabelle durch
#' \code{markLeft1}, \code{markValue1} und \code{markRight1} ersetzt.
#' @param markValue1 character oder NULL: falls NULL, wird der jeweilige Wert
#' von \code{estimator} nicht ueberschrieben.
#' @param markLeft1 character: wird links zu \code{markValue1} hinzugefuegt.
#' @param markRight1 character: wird rechts zu \code{markValue1} hinzugefuegt.
#' @param lim2 numerischer Wert: falls \code{lim2}>\code{error}, wird der
#' entsprechende Wert von \code{estimator} in der Tabelle durch
#' \code{markLeft2}, \code{markValue2} und \code{markRight2} ersetzt.
#' @param markValue2 character oder NULL: falls NULL, wird der jeweilige Wert
#' von \code{estimator} nicht ueberschrieben.
#' @param markLeft2 character: wird links zu \code{markValue2} hinzugefuegt.
#' @param markRight2 character: wird rechts zu \code{markValue2} hinzugefuegt.
#' @return Output ist eine Tabelle in Matrix-Form nach demselben System wie die
#' meisten Tabellen im MZ-Quartalsbericht - mit durch \code{col} und \code{row}
#' definierten Spalten und Zeilen fuer bestimmte Zeitpunkte und Veraenderungen.
#' @seealso
#' \code{\link{MakeTable},\link{FillExcelTemplate},\link{ImportDataListQT},\link{ImportData},\link{IndivImportData},\link{ImportAndMerge}}
#' @export
#' @examples
#' \dontrun{
#' ### Daten einlesen
#' 
#' datalist <- ImportDataListQT(timeInstant=c(2014,4),whichVar=c("rbpkin"))
#' 
#' ### Tabelle A1 im Quartalsbericht:
#' 
#' ### Spalten
#' col <- list()
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat!=4", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat!=4 & balt>=15", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat!=4 & balt>=15 & balt<=64", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat%in%c(1,2) & balt>=15", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat%in%c(1,2) & balt>=15 & balt<=64", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupRate",
#'   TFstring="xerwstat%in%c(1,2) & balt>=15 & balt<=64",
#'   TFstring2="xerwstat!=4 & balt>=15 & balt<=64")
#' 
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat==1 & balt>=15", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat==1 & balt>=15 & balt<=64", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupRate",
#'   TFstring="xerwstat==1 & balt>=15 & balt<=64",
#'   TFstring2="xerwstat!=4 & balt>=15 & balt<=64")
#' 
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat==2 & balt>=15", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupRate",
#'   TFstring="xerwstat==2 & balt>=15",
#'   TFstring2="xerwstat%in%c(1,2) & balt>=15 & balt<=74")
#' 
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat==3 & balt>=15", scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",
#'   TFstring="xerwstat==3 & balt>=15 & balt<=64", scaleF="/1000")
#' 
#' ### Zeilen
#' row <- list(NULL,each="bsex")  
#' 
#' ### Erstellen 2 Tabellen fuer FillExcelTemplate();
#' ### einmal mit und einmal ohne Limits (enthaelt gegebenenfalls characters)
#' # Tabelle 1: Mit Limits
#' tab1 <- MakeQT(datalist,col=col,row=row,timeInstant=c(2014,4),
#'   lim1=0.17,lim2=0.25)
#' # Tabelle 2: Ohne Limits (enthaelt nur numerische Werte)
#' tab2 <- MakeQT(datalist,col=col,row=row,timeInstant=c(2014,4))
#' }
#' 



#saveRDS(datalist,"//DatenB/B_MZ/AKE Neu ab 2004/06 Ergebnisse/Quartalsberichte Produktion/datalist_qt.Rds")

MakeQT <- function(datalist,col,row=NULL,timeInstant,
                   lim1=Inf,markLeft1="(",markRight1=")",markValue1=NULL,
                   lim2=Inf,markLeft2="(",markRight2=")",markValue2="x"){
  
  
  ende <- timeInstant  
  
  tInterval <- date_decimal(as.numeric(time(ts(start=start(lag(ts(end=ende,frequency=4),3)),end=ende,frequency=4))))
  jahr_seq <- year(tInterval)
  quartal_seq <- quarter(tInterval)
  
  vjq <- date_decimal(as.numeric(time(lag(ts(end=ende,frequency=4),4))))
  vjq_jahr <- year(vjq)
  vjq_quartal <-  quarter(vjq)
  
  vvjq <- date_decimal(as.numeric(time(lag(ts(end=ende,frequency=4),8))))
  vvjq_jahr <- year(vvjq)
  vvjq_quartal <-  quarter(vvjq)
  
  timeInstants_jahr <- c(vvjq_jahr,jahr_seq)
  timeInstants_quartal <- c(vvjq_quartal,quartal_seq)
  # brauche das jew. vierte Element dieser timeInstants_jahr und timeInstants_quartal Vektoren eigentlich nicht weil das ueber comp_diff_lag abgedeckt wird
  # verwende es aber spaeter noch fuer was andres, lass es also erst mal drinnen
  
  res <- list()
  for(i in 1:length(timeInstants_jahr)){
    jahr <- timeInstants_jahr[i]
    quartal <- timeInstants_quartal[i]
    
    if(jahr==ende[1]&&quartal==ende[2]){
      ### Aktuelles Quartal
      dat <- datalist[[paste0("dat_",jahr,"q",quartal,"vq")]]
      res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, error="cv", lim1=lim1,lim2=lim2)
      names(res)[length(res)] <- paste0(quartal,". Quartal ", jahr)
      ### Vorquartal
      res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, estimator="estPrev",error="cv", lim1=lim1,lim2=lim2)
      names(res)[length(res)] <- paste0(quartal_seq[length(quartal_seq)-1],". Quartal ", jahr_seq[length(jahr_seq)-1])
      if(lim1==Inf && lim2==Inf){
        res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, estimator="absChange",error="cv", lim1=lim1,lim2=lim2)
      }else{
        res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, estimator="absChange",error="ci",markLeft1 = "", markRight1 = "*")
      }
      names(res)[length(res)] <- paste0(quartal,". Quartal ", jahr," Diffvq")
      rm(dat);gc()
      
      ### Vorjahresquartal
      dat <- datalist[[paste0("dat_",jahr,"q",quartal,"vjq")]]
      res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, estimator="estPrev", error="cv", lim1=lim1,lim2=lim2)
      names(res)[length(res)] <- paste0(vjq_quartal,". Quartal ", vjq_jahr)
      if(lim1==Inf && lim2==Inf){
        res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, estimator="absChange", error="cv",lim1=lim1,lim2=lim2)
      }else{
        res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, estimator="absChange", error="ci",markLeft1 = "", markRight1 = "*")
      }
      names(res)[length(res)] <- paste0(quartal,". Quartal ", jahr," Diffvjq")
      rm(dat);gc()
    }else if(!identical(c(jahr,quartal),c(jahr_seq[length(jahr_seq)-1] ,quartal_seq[length(quartal_seq)-1]))){
      ### Rest
      dat <- datalist[[paste0("dat_",jahr,"q",quartal)]]
      res[[length(res)+1]] <- MakeTable(dat,col=col,row=row, error="cv", lim1=lim1,lim2=lim2)
      names(res)[length(res)] <- paste0(quartal,". Quartal ", jahr)
      rm(dat);gc()
    }
  }
  ## res umsortieren:
  res <- res[c(paste0(vvjq_quartal,". Quartal ", vvjq_jahr),paste0(vjq_quartal,". Quartal ", vjq_jahr),
               paste0(quartal_seq,". Quartal ", jahr_seq),
               paste0(quartal,". Quartal ", jahr," Diffvjq"),paste0(quartal,". Quartal ", jahr," Diffvq"))]
  
  nbloecke <- nrow(na.omit(res[[1]]))
  bloecke <- list()
  
  
  ## neue Bloecke formen:
  for(i in 1:nbloecke){
    bloecke[[length(bloecke)+1]] <- do.call(rbind,lapply(res,function(x)na.omit(x)[i,]))
  }
  
  
  (ergebnis <- do.call(rbind,bloecke))  
  
  return(ergebnis)
}





