quantNA <- function(x,...){
  quantile(x,na.rm=TRUE,...)
}
makeEachVar <- function(x){
  x <- x[length(x):1]
  out <- 0
  for(i in 1:length(x)){
    out <- out + x[i]*10^((i-1)*3)
  }
  return(out)
}
GroupSizeX <- function(x,TFstring){
  gew1 <- sd <- NULL

  if(length(x)==1){
    date <- gsub("dat_","",names(x))
    x <- x[[1]]
    y <- NULL
    if(!"gew1"%in%names(x)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
  }else{
    date <- gsub("dat_","",names(x)[1])
    datePrev <- gsub("dat_","",names(x)[2])
    y <- x[[2]]
    x <- x[[1]]
    if(!"gew1"%in%names(x) || !"gew1"%in%names(y)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
  }
  bw <- colnames(x)[grepl("gew1_",colnames(x))]
  if(length(bw)==0){
    stop("Es wurden keine Bootstrapgewichte uebergeben! Moeglicherweise liegt es auch an den Variablennamen der Bootstrapgewichte, diese muessen die Namen 'gew1_1', 'gew1_2', 'gew1_3', ... haben.\n")
  }
  est <- x[eval(parse(text=TFstring)),sum(gew1)]
  estb <- x[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bw]
  sde <- sd(estb)
  if(is.null(y)){
    return(list(date=date,est=est,sd=sde,cv=sde/est,cil=quantNA(estb,.025),ciu=quantNA(estb,.975)))
  }else{
    est2 <- y[eval(parse(text=TFstring)),sum(gew1)]
    estb2 <- y[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bw]
    sde2 <- sd(estb2)
    absdiff <- est-est2
    absdiffb <- estb-estb2
    ratediff <- 100*(est-est2)/est2
    ratediffb <- 100*(estb-estb2)/estb2
    sdabs <- sd(absdiffb)
    sdrel <- sd(ratediffb)
    return(list(date=date,est=est,sd=sde,cv=sde/est,cil=quantNA(estb,.025),ciu=quantNA(estb,.975),
                datePrev=datePrev,estPrev=est2,sdPrev=sde2,cvPrev=sde2/est2,cilPrev=quantNA(estb2,.025),ciuPrev=quantNA(estb2,.975),
                absChange=absdiff,sdAbsChange=sdabs,cvAbsChange=sdabs/absdiff,cilAbsChange=quantNA(absdiffb,.025),ciuAbsChange=quantNA(absdiffb,.975),
                relChange=ratediff,sdRelChange=sdrel,cvRelChange=sdrel/ratediff,cilRelChange=quantNA(ratediffb,.025),ciuRelChange=quantNA(ratediffb,.975)
    ))
  }
}
GroupRateX <- function(x,TFstring,TFstring2=NULL){
  gew1 <- sd <- NULL
  if(length(x)==1){
    date <- gsub("dat_","",names(x))
    x <- x[[1]]
    y <- NULL
    if(!"gew1"%in%names(x)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
  }else{
    date <- gsub("dat_","",names(x)[1])
    datePrev <- gsub("dat_","",names(x)[2])    
    y <- x[[2]]
    x <- x[[1]]
    if(!"gew1"%in%names(x) || !"gew1"%in%names(y)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
  }
  bw <- colnames(x)[grepl("gew1_",colnames(x))]
  # if(grepl("gew1",colnames(x),fixed=TRUE))
  if(length(bw)==0){
    stop("Es wurden keine Bootstrapgewichte uebergeben! Moeglicherweise liegt es auch an den Variablennamen der Bootstrapgewichte, diese muessen die Namen 'gew1_1', 'gew1_2', 'gew1_3', ... haben.\n")
  }
  num <- x[eval(parse(text=TFstring)),sum(gew1)]
  numb <- x[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bw]
  if(any(dim(numb)==0))
    numb <- rep(0,length(bw))
  if(is.null(TFstring2)){
    denum <- x[,sum(gew1)]
    denumb <- x[,lapply(.SD,sum),.SDcols=bw]
  }else{
    denum <- x[eval(parse(text=TFstring2)),sum(gew1)]
    denumb <- x[eval(parse(text=TFstring2)),lapply(.SD,sum),.SDcols=bw]
    if(any(dim(denumb)==0))
      denumb <- rep(0,length(bw))
  }
  est <- 100*num/denum
  estb <- 100*numb/denumb
  sde <- sd(estb)
  if(is.null(y)){
    return(list(date=date,est=est,sd=sde,cv=sde/est,cil=quantNA(estb,.025),ciu=quantNA(estb,.975)))  
  }else{
    num2 <- y[eval(parse(text=TFstring)),sum(gew1)]
    numb2 <- y[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bw]
    if(any(dim(numb2)==0))
      numb2 <- rep(0,length(bw))
    if(is.null(TFstring2)){
      denum2 <- y[,sum(gew1)]
      denumb2 <- y[,lapply(.SD,sum),.SDcols=bw]
    }else{
      denum2 <- y[eval(parse(text=TFstring2)),sum(gew1)]
      denumb2 <- y[eval(parse(text=TFstring2)),lapply(.SD,sum),.SDcols=bw]
      if(any(dim(denumb2)==0))
        denumb2 <- rep(0,length(bw))
    }
    est2 <- 100*num2/denum2
    estb2 <- 100*numb2/denumb2
    sde2 <- sd(estb2)
    absdiff <- est-est2
    absdiffb <- estb-estb2
    ratediff <- 100*(est-est2)/est2
    ratediffb <- 100*(estb-estb2)/estb2
    sdabs <- sd(absdiffb)
    sdrel <- sd(ratediffb)
    return(list(date=date,est=est,sd=sde,cv=sde/est,cil=quantNA(estb,.025),ciu=quantNA(estb,.975),
                datePrev=datePrev,estPrev=est2,sdPrev=sde2,cvPrev=sde2/est2,cilPrev=quantNA(estb2,.025),ciuPrev=quantNA(estb2,.975),
                absChange=absdiff,sdAbsChange=sdabs,cvAbsChange=sdabs/absdiff,cilAbsChange=quantNA(absdiffb,.025),ciuAbsChange=quantNA(absdiffb,.975),
                relChange=ratediff,sdRelChange=sdrel,cvRelChange=sdrel/ratediff,cilRelChange=quantNA(ratediffb,.025),ciuRelChange=quantNA(ratediffb,.975)
    ))
  }
}

#' @export
#' @rdname GroupSize
GroupRate <- function(x,TFstring,TFstring2=NULL,each=NULL,byeach=TRUE,thousands_separator=TRUE,digits=2){
  GroupX(x=x,TFstring=TFstring,TFstring2=TFstring2,each=each,byeach=byeach,thousands_separator=thousands_separator,digits=digits,method="GroupRate")
}


#' Schaetz- und Fehlerrechnungsfunktionen.
#' 
#' Funktion berechnet absolute (\code{GroupSize}) oder relative
#' (\code{GroupRate}) Schaetzwerte und die zugehoerigen Fehler.
#' 
#' Wiedergegeben wird der Schaetzer \code{est}, der Stichprobenfehler
#' \code{sd}, der Variationskoeffizient \code{cv} und die untere/obere Grenze
#' des 95\% Konfidenzintervalls \code{cil_2.5\%}/\code{ciu_97.5\%}.
#' Die Fehler werden mit Hilfe von Replikationsgewichten aus einem Bootstrapverfahren
#' berechnet, d.h. \code{sd} entspricht der Standardabweichung der mit den Bootstrapgewichten
#' berechneten Schaetzwerte, \code{cil_2.5\%} und \code{ciu_97.5\%} sind die 
#' entsprechenden 2.5\% und 97.5\% Quantile und \code{cv=sd/est}.
#' 
#' @aliases GroupSize GroupRate
#' @param x MZ Daten - Output von Funktion \link{ImportData}.
#' @param TFstring Character oder NULL: Logische Einschraenkung der Gruppe, im Fall der Anteile (\code{GroupRate}): 
#' Einschraenkung fuer Zaehler. Falls NULL, gilt keine Einschraenkung.
#' @param TFstring2 Character oder NULL: Logische Einschraenkung fuer den Nenner. 
#' Falls NULL, gilt keine Einschraenkung fuer den Nenner ausser wenn \code{each} ungleich NULL und \code{byeach=TRUE}.
#' @param each Character oder NULL: Name der Variable nach der getrennt berechnet werden soll. 
#' Hier koennen auch mehrere Variablen angegeben werden und zwar in einem character string, getrennt durch ein +, siehe Examples.
#' @param byeach Logical: Vorgabe fuer den Nenner. \code{byeach} bezieht sich auf den 
#' Parameter \code{each} und gibt an, ob bei \code{GroupRate} zusaetzlich zu 
#' \code{TFstring2} auch die jeweilige durch \code{each} definierte Einschraenkung 
#' in den Nenner kommen soll, also ob die Raten jeweils bezogen auf die Auspraegungen 
#' bzw. Auspraegungskombinationen von \code{each} berechnet werden sollen (\code{byeach=TRUE}) 
#' oder rein bezogen auf \code{TFstring2} (\code{byeach=FALSE}). 
#' @param thousands_separator Logical: Wenn TRUE, werden Tausendertrennzeichen
#' angezeigt.
#' @param digits Numerischer Wert: Anzahl der Nachkommastellen im angezeigten Ergebnis. Default
#' ist 2.
#' @return Output ist ein Objekt der Klasse \code{mzR}.
#' @seealso
#' \code{\link{ImportData},\link{IndivImportData},\link{ImportAndMerge},\link{GetLabels},\link{Total},\link{Mean},\link{export}}
#' @examples
#' # Daten laden (oder Daten einlesen mit ImportData() bzw. IndivImportData())
#' data(mzTestData)
#' 
#' # Arbeitslosenzahlen: Absolutwerte und Veraenderung
#' GroupSize(mzTestData,TFstring="xerwstat==2&balt>=15&balt<=74")
#' # Arbeitslosenquoten: Prozentwerte und Veraenderung 
#' GroupRate(mzTestData,TFstring="xerwstat==2&balt>=15&balt<=74",
#'           TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74")
#' # Oesterreichische Bevoelkerung nach Bundesland und Geschlecht
#' GroupSize(mzTestData,TFstring=NULL,each="xnuts2+bsex")
#' 
#' 
#' \dontrun{
#' ############################################################################################
#' #   Zusaetzliche Beispiele fuer DatennutzerInnen der Mikrozensus-Arbeitskraefteerhebung:   #
#' ############################################################################################
#' # Quartal und zugehoeriges Vorjahrsquartal einlesen (Funktion fuer STAT-interne Nutzer)
#' dat <- ImportData(year=2014,quarter=4, comp_diff_lag=4)
#' 
#' # Oesterreichische Bevoelkerung nach Bundesland und Geschlecht
#' GroupSize(dat,TFstring=NULL,each="xnuts2+bsex")
#' 
#' # Arbeitslosenzahlen: Absolutwerte und Veraenderung
#' GroupSize(dat,TFstring="xerwstat==2&balt>=15&balt<=74")
#' # Arbeitslosenquoten: Prozentwerte und Veraenderung 
#' GroupRate(dat,TFstring="xerwstat==2&balt>=15&balt<=74",
#'   TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74")
#' # Arbeitslosenzahl vom aktuelleren der beiden Quartale
#' GroupSize(dat[1],TFstring="xerwstat==2&balt>=15&balt<=74")
#' # Arbeitslosenquote vom weniger aktuellen Quartal
#' GroupRate(dat[2],TFstring="xerwstat==2&balt>=15&balt<=74",
#'   TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74")
#' 
#' # Absolutwerte und Veraenderung fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
#' GroupSize(dat,TFstring="balt>=15&balt<=74",each="xerwstat")
#' # Prozentwerte (bezogen auf Gesamtbevoelkerung) und Veraenderung 
#' # fuer jede Auspraegung von xerwstat eingeschraenkt auf 15-74-Jaehrige
#' GroupRate(dat,TFstring="balt>=15&balt<=74",each="xerwstat")
#' # Arbeitslosenzahlen: Absolutwerte und Veraenderung fuer jedes Bundesland
#' GroupSize(dat,TFstring="xerwstat==2&balt>=15&balt<=74",each="xnuts2")
#' 
#' # Arbeitslosenquote: Prozentwerte und Veraenderung pro Bundesland
#' GroupRate(dat,TFstring="xerwstat==2&balt>=15&balt<=74",
#'   TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74",each="xnuts2")
#' # Arbeitslosenquote: Prozentwerte und Veraenderung pro Geschlecht
#' GroupRate(dat,TFstring="xerwstat==2&balt>=15&balt<=74",
#'   TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74",each="bsex")
#' # Arbeitslosenquote: Prozentwerte und Veraenderung pro Bundesland X Geschlecht
#' GroupSize(dat,TFstring="xerwstat==2&balt>=15&balt<=74",each="xnuts2+bsex")
#' GroupRate(dat,TFstring="xerwstat==2&balt>=15&balt<=74",
#'   TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74",each="xnuts2+bsex")
#' 
#' # Haushalte: Quartal und Vorquartal einlesen.
#' dat <- ImportData(year=2014,quarter=4, comp_diff_lag=1, hh=TRUE)
#' # Absolutwerte: Anzahl der Hauptmietwohnungen ohne gueltiger Kostenangabe.
#' GroupSize(dat,TFstring="wrecht==3")
#' GroupSize(dat,TFstring="wrecht2%in%c(1:3)")
#' }
#' 
#' @export GroupSize
GroupSize <- function(x,TFstring=NULL,each=NULL,thousands_separator=TRUE,digits=2){
  GroupX(x=x,TFstring=TFstring,each=each,thousands_separator=thousands_separator,digits=digits,method="GroupSize")
}  
GroupX <- function(x,TFstring,TFstring2=NULL,each=NULL,byeach=TRUE,thousands_separator=TRUE,digits=2,method){
  if(is.null(TFstring)){
    TFstring <- TRUE  
  }
  if(is.null(each)){
    if(method=="GroupSize")
      res <- GroupSizeX(x,TFstring)
    else
      res <- GroupRateX(x,TFstring,TFstring2)
    class(res) <- "mzR"
    attr(res,"each") <- NULL
    attr(res,"thousands_separator") <- thousands_separator
    attr(res,"digits") <- digits
    if(method=="GroupSize"){
      attr(res,"ergType") <- "GroupSize"
      attr(res,"TFstring") <- TFstring
    }else{
      attr(res,"ergType") <- "GroupRate"
      attr(res,"TFstring") <- TFstring
      attr(res,"TFstring2") <- TFstring2
      attr(res,"byeach") <- byeach
    }
    # if(any(is.na(res))){
    #   warning("\nAchtung, moeglicherweise fehlen zu einer oder mehreren Beobachtungen Bootstrapgewichte!\n")
    # }
    return(res)
  }
  res <- list()
  if(length(grep("\\+",each))>0){
    eachv <- strsplit(each,"\\+")[[1]]
    eachvar <- paste(eachv,collapse="_")
    for(i in 1:length(x)){
      x[[i]][[eachvar]] <- apply(x[[i]][,eachv,with=FALSE],1,makeEachVar)
    }
  }else{
    eachv <- eachvar <- each
  }
  for(l in x[[1]][,sort(unique(eval(parse(text=eachvar))))]){
    TFstringcur <- paste0(eachvar,"==",l,"&",TFstring)
    if(method=="GroupSize"){
      res[[paste0(eachvar,"_",l)]] <- GroupSizeX(x,TFstringcur)
    }else{
      if(byeach){
        if(!is.null(TFstring2))
          TFstringcur2 <- paste0(eachvar,"==",l,"&",TFstring2)
        else
          TFstringcur2 <- paste0(eachvar,"==",l)
      }else{
        TFstringcur2 <- TFstring
      }
      res[[paste0(eachvar,"_",l)]] <- GroupRateX(x,TFstringcur,TFstringcur2)
    }
    res[[paste0(eachvar,"_",l)]][["each"]] <- head(x[[1]][eval(parse(text=TFstringcur)),eachv,with=FALSE],1)
  }
  
  class(res) <- "mzR"
  attr(res,"each") <- each
  attr(res,"thousands_separator") <- thousands_separator
  attr(res,"digits") <- digits
  if(method=="GroupSize"){
    attr(res,"ergType") <- "GroupSize"
    attr(res,"TFstring") <- TFstring
  }else{
    attr(res,"ergType") <- "GroupRate"
    attr(res,"TFstring") <- TFstring
    attr(res,"TFstring2") <- TFstring2
    attr(byeach,"TFstring2") <- byeach
  }
  # if(any(is.na(unlist(res)))){
  #   warning("\nAchtung, moeglicherweise fehlen zu einer oder mehreren Beobachtungen Bootstrapgewichte!\n")
  # }
  return(res)
}
