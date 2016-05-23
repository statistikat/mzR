createDes <- function(x,bw){
  return(svrepdesign(repweights=as.formula(paste("~",paste(bw,collapse="+"))),
          weights=~gew1, combined.weights=TRUE,type="bootstrap",data=x))
}


#' Hilfsfunktion zur Verwendung des survey-Pakets.
#' 
#' Erstellt ein svyrepdesign Objekt aus den eingelesen Daten
#' 
#' Under construction.
#' 
#' @param x MZ Daten - Output von Funktion ImportData()
#' @return Under construction.
#' @seealso \code{\link{ImportData},\link{IndivImportData}}
#' @examples
#' 
#' \dontrun{
#' dat <- ImportData(year=2014,quarter=4, comp_diff=TRUE, comp_diff_lag=4)
#' des <- Design(dat)
#' }
#' 
#' @export Design
Design <- function(x){
  bw <- colnames(x[[1]])[grepl("gew1_",colnames(x[[1]]))]
  return(lapply(x,createDes,bw))
}
