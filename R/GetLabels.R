#' Labels einer Variablen wiedergeben.
#' 
#' Funktion zeigt SPSS-Labels einer Variablen an.
#' 
#' 
#' @param x MZ Daten - Output von Funktion \link{ImportData} oder \link{IndivImportData}.
#' @param var Character Vektor mit Variable(n) fuer die die Labels angezeigt
#' werden sollen.
#' @param curr TRUE/FALSE ob \code{var} auf "current" oder "previous" Zeitpunkt
#' bezogen sein soll, d.h. falls \code{comp_diff_lag} ungleich NULL in der
#' Funktion \code{ImportData}, koennen mit \code{curr=FALSE} auch die Labels
#' des weniger aktuellen der beiden Datensaetze ausgegeben werden.
#' \code{curr=TRUE} bezieht sich auf den aktuelleren der beiden Zeitpunkte.
#' @return Output sind die Labels einer Variablen \code{var} falls im
#' dg7-Datensatz welche definiert wurden.
#' @seealso
#' \code{\link{ImportData},\link{IndivImportData},\link{ImportAndMerge}}
#' @examples
#' # Daten laden (oder Daten einlesen mit ImportData() bzw. IndivImportData())
#' data(mzTestData)
#' 
#' # Labels von Variable xerwstat abfragen
#' GetLabels(mzTestData,"xerwstat")
#' # Labels von Variablen dtstd und xnuts2 abfragen
#' GetLabels(mzTestData,c("dtstd","xnuts2"))
#' 
#' @export GetLabels
GetLabels <- function(x,var,curr=TRUE){
  
  if(curr){
    x <- x[1][[1]]
  }
  if(!(curr)){
    if(length(x)<2){
      stop(paste0("Nur ",names(x)," geladen, Aufruf f\u00FCr 'prev' nicht m\u00F6glich."))
    }      
    x <- x[2][[1]]
  }
  if(!all(var%in%names(x))){
    stop(paste0(var[which(!var%in%names(x))], " ist im Datensatz nicht enthalten!"))
  }
  
  res <- list()
  for(i in var){
    if(is.null(attributes(x[[i]])[c("label","value.labels")])){
      res[[length(res)+1]] <- paste0("Es gibt keine Labels zu ",i,"")
      names(res)[length(res)] <- i      
    }else{
      res[[length(res)+1]] <- attributes(x[[i]])[c("label","value.labels")]
      names(res)[length(res)] <- i
    }
  }
  return(res)
}
