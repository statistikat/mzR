#' @export
#' @rdname ExportFehler
export <- function(x,outFilePath=getwd(),outFileName=NULL)
  UseMethod("export")

#' Export der Schaetz-und Fehlerrechnungsergebnisse.
#' 
#' Funktion exportiert die Ergebnisse der Schaetz-und Fehlerrechnungsfunktionen
#' in ein .csv-File.
#' 
#' 
#' @aliases export.mzR export
#' @param x Ergebnis von GroupSize, GroupRate, Total oder Mean.
#' @param outFilePath Pfad, wo die CSV-Datei gespeichert werden soll. Default
#' ist das aktuelle Work Directory.
#' @param outFileName Name der CSV-Datei in die exportiert werden soll.
#' @return Ein .csv-File mit den Ergebnissen wird in \code{out_file_pfad}
#' abgespeichert.
#' @seealso
#' \code{\link{ImportData},\link{IndivImportData},\link{GetLabels},\link{GroupSize},\link{GroupRate},\link{Total},\link{Mean}}
#' @export
#' @rdname ExportFehler
#' @examples
#' # Daten laden (oder Daten einlesen mit ImportData() bzw. IndivImportData())
#' data(mzTestData)
#'
#' # Arbeitslosenzahlen (Absolutwerte)
#' ergebnis <- GroupSize(mzTestData,TFstring="xerwstat==2&balt>=15&balt<=74")
#' \dontrun{
#' # Arbeitslosenzahlen (Absolutwerte) ins Working Directory (zu erfragen mit getwd()) exportieren
#' export(ergebnis)
#' # Erwerbsstatus nach Bundesland ins Working Directory exportieren
#' export(GroupSize(dat,each="xerwstat+xnuts2"))
#' }
#' 
export.mzR <- function(x,outFilePath=getwd(),outFileName=NULL){
  attr(x,"thousands_separator") <- FALSE
  digits <- attr(x,"digits")
  each <- attr(x,"each")
  ergType <- attr(x,"ergType")
  TFstring <- attr(x,"TFstring")
  TFstring2 <- attr(x,"TFstring2")
  ergVar <- attr(x,"var")
  if(isTRUE(TFstring)){
    TFstring <- "Gesamtbev\u00F6lkerung"
  }
  
  if(is.null(each)){
    date <- paste(unlist(x[grep("date",names(x),value=TRUE)]),collapse=" ")
  }else{      
    date <- paste(unlist(x[[1]][grep("date",names(x[[1]]),value=TRUE)]),collapse=" ")
  }
  
  cat("\nNachfolgende Ergebnisse werden exportiert...\n")

    if(ergType=="GroupSize" | ergType=="Total"){ 
      cat("\nAbsolutwerte f\u00FCr ",TFstring,":\n\n",sep="")
      if(!is.null(ergVar)){
        est_Info <- c("est_Info:",paste("Absolutwerte",ergVar),TFstring,date)
      }else{
        est_Info <- c("est_Info:","Absolutwerte",TFstring,date)
      }
      
    }
    if(ergType=="GroupRate"){  
      cat("\nProzentwerte f\u00FCr ",TFstring," im Z\u00E4hler \n",sep="")
      if(!is.null(TFstring2)){
        cat("und ",TFstring2," im Nenner:\n\n",sep="")
      est_Info <- c("est_Info:","Prozentwerte",paste("Z\u00E4hler:",TFstring),paste("Nenner:",TFstring2),date)
      }else{
        cat("und der Gesamtbev\u00F6lkerung im Nenner:\n\n",sep="")    
      est_Info <- c("est_Info:","Prozentwerte",paste("Z\u00E4hler:",TFstring),paste("Nenner:","Gesamtbev\u00F6lkerung"),date)
      }  
      
    } 
    if(ergType=="Mean"){ 
      cat("\nMittelwerte f\u00FCr ",TFstring,":\n\n",sep="")
      est_Info <- c(TFstring,rep("",4))
      if(!is.null(ergVar)){
        est_Info <- c("est_Info:",paste("Mittelwerte",ergVar),TFstring,date)
      }else{
        est_Info <- c("est_Info:","Mittelwerte",TFstring,date)
      }      
    }    
  
   
  if(is.null(each)){
    x <- as.matrix(print(x))
  }else{
    x <- as.matrix(do.call(rbind,print(x)))
    if(nchar(date)<10){
      rownames(x) <- paste(rownames(x),date,sep=".") 
    }
  }
 
  est_Info_matrix <- matrix(NA,length(est_Info)+1,ncol(x))
  rownames(est_Info_matrix) <- c("",est_Info)
  x <- rbind(x,est_Info_matrix)
  
  
  
  if(is.null(outFileName)){
    if(ergType=="GroupSize" | ergType=="Total")  
      outFileName <- paste0("Fehlerrechnung Absolutwerte ",date)
    if(ergType=="GroupRate")  
      outFileName <- paste0("Fehlerrechnung Prozentwerte ",date)  
    if(ergType=="Mean")  
      outFileName <- paste0("Fehlerrechnung Mittelwerte ",date)  
    
  }
  write.csv2(x,paste0(outFilePath,"/",outFileName,".csv"),na="",fileEncoding="Latin1")
  
  cat("\n'",paste0(outFileName,".csv'"), " wurde in ", outFilePath," abgespeichert.\n",sep="")
}


