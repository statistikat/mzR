#' Neue Variable zu den Daten hinzufuegen.
#' 
#' Funktion erzeugt neue Variable in MZ-Daten.
#' 
#' Um in den Daten eine neue Variable \code{newVar} zu erzeugen, z.B. durch Rekodierungen einer anderen Variable,
#' muss man eine Funktion \code{functionName} definieren. Dieser Funktion uebergibt man als Funktionsparameter die
#' Variablenname(n) der in den Daten vorhandenen Merkmale die man zum Erzeugen der neuen Variable braucht. 
#' Mehr dazu in den Beispielen.
#'  
#' 
#' @param x MZ Daten - Output von Funktion \link{ImportData} oder \link{IndivImportData}.
#' @param functionName Character: Name der Funktion mit der die neue Variable \code{newVar} erzeugt werden soll. Siehe Details.
#' @param newVar Character: Name der neu zu erzeugenden Variable.
#' @return Output sind wieder die MZ-Daten \code{x} die um die neu erzeugte Variable \code{newvar} ergaenzt wurden.
#' @seealso
#' \code{\link{ImportData},\link{IndivImportData}}
#' @examples
#' # Daten laden (oder Daten einlesen mit ImportData() bzw. IndivImportData())
#' data(mzTestData)
#' 
#' # Zuerst wird jeweils die Funktion definiert mit der die neue Variable erzeugt werden soll.
#' # Dann wird mit AddVariable diese Funktion auf die Daten angewandt. 
#' 
#' # Beispiel 1:
#' xerw_fun <- function(balt, xerwstat) {
#' x <- ifelse(balt>=15 & balt <=24, xerwstat, -99)
#' return(x)
#' }
#' mzTestData <- AddVariable(x=mzTestData, functionName=xerw_fun, newVar="xerw")
#' 
#' # Beispiel 2:
#' stdrec_fun <- function(dtstd, estund){
#'   x <- vector()
#'   x[dtstd==999] <- 999
#'   x[dtstd==-3] <- -3
#'   x[dtstd!=999] <- dtstd[dtstd!=999] 
#'   x[estund!=-3 & dtstd!=-3 & dtstd!=999] <- 
#'   dtstd[estund!=-3 & dtstd!=-3 & dtstd!=999]+estund[estund!=-3 & dtstd!=-3 & dtstd!=999]
#'   return(x)
#' }
#' mzTestData <- AddVariable(x=mzTestData, functionName=stdrec_fun, newVar="stdrec")
#' 
#' \dontrun{
#' # Beispiel 3:
#' age_pub_fun <- function(balt){
#'   x <- balt
#'   x <- car::recode(x, "-3= -3; 0:14=0; 15:19=1; 20:24=2; 
#'   25:34=3; 35:44=4; 45:54=5; 55:59=6; 60:64=7; 65:200=8")
#'   return(x)
#' }
#' mzTestData <- AddVariable(x=mzTestData, functionName=age_pub_fun, newVar="age_pub")
#' }
#' 
#' @export AddVariable

AddVariable <- function(x, functionName, newVar) {
  args <- formalArgs(functionName)
  for (i in 1:length(x)) {
    cmd <- paste0("x[[i]][,",newVar,":=functionName(",paste0(args,collapse=", "),")]")
    eval(parse(text=cmd))
  }
  x
}


