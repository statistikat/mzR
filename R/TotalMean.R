appendVarNumTmp <- function(x, var, negativeZero) {
  if(length(grep("\\+",var))>0||length(grep("\\*",var))>0||length(grep("\\-",var))>0||length(grep("\\/",var))>0){
    if(negativeZero){
      vars <- unlist(strsplit(var,"\\+"))
      vars <- unlist(strsplit(vars,"\\*"))
      vars <- unlist(strsplit(vars,"\\-"))
      vars <- unlist(strsplit(vars,"\\/"))
      vars <- unlist(strsplit(vars,"\\("))
      vars <- unlist(strsplit(vars,"\\)"))
      vars <- vars[vars != ""]
      for(v in vars){
        x[eval(parse(text = quote(v))) < 0, eval(parse(text = quote(v))) := 0]  
      }
    }
    x[,':='(varNumTmp = eval(parse(text=var)))]
  }else{
    x[,':='(varNumTmp = eval(parse(text=var)))]
    if(negativeZero){
      x[varNumTmp < 0, varNumTmp := 0]
    }
  }
}

TotalX <- function(xx, TFstring, var, negativeZero = TRUE, replicates){
  gew1Num <- varNumTmp <- bwTmp <- gew1 <- NULL
  bw <- colnames(xx[[1]])[grepl("gew1_",colnames(xx[[1]]))]
  if(length(bw)==0){
    stop("Es wurden keine Bootstrapgewichte uebergeben! Moeglicherweise liegt es auch an den Variablennamen der Bootstrapgewichte, diese muessen die Namen 'gew1_1', 'gew1_2', 'gew1_3', ... haben.\n")
  }
  bwNum <- paste0(bw,"Num")
  date <- gsub("dat_","",names(xx))
  x <- copy(xx[[1]])
  y <- NULL
  if(!"gew1"%in%names(x)){
    stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
  }
  
  appendVarNumTmp(x, var, negativeZero)
  
  xvNTVector <- x[['varNumTmp']] ## Quick fix, sollte evt keine permanente Loesung sein
  x[,gew1Num:=gew1*varNumTmp]
  x[, (bw) := lapply(.SD, function(y){y * xvNTVector}), .SDcols = bw]
  setnames(x,old=bw,new=bwNum)
  
  if(length(xx)>1){
    date <- gsub("dat_","",names(xx)[1])
    datePrev <- gsub("dat_","",names(xx)[2])
    y <- copy(xx[[2]])
    if(!"gew1"%in%names(y)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
    appendVarNumTmp(y, var, negativeZero)
    
    y[,gew1Num:=gew1*varNumTmp]
    
    yvNTVector <- y[['varNumTmp']] ## Quick fix, sollte evt keine permanente Loesung sein
    y[, (bw) := lapply(.SD, function(z){z * yvNTVector}), .SDcols = bw]
    setnames(y,old=bw,new=bwNum)
  }
  est <- x[eval(parse(text=TFstring)),sum(gew1Num)]
  estb <- x[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bwNum]
  sde <- sd(estb)
  if(is.null(y)){
    return(mzRComponent(date, est, estb, returnBR = replicates))
  }else{
    est2 <- y[eval(parse(text=TFstring)),sum(gew1Num)]
    estb2 <- y[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bwNum]
    return(mzRComponent2(date, est, est2, estb, estb2, datePrev, returnBR = replicates))
  }
}
MeanX <- function(xx, TFstring, var, negativeZero = TRUE, replicates = replicates){
  gew1Num <- varNumTmp <- bwTmp <- gew1 <- NULL
  bw <- colnames(xx[[1]])[grepl("gew1_",colnames(xx[[1]]))]
  if(length(bw)==0){
    stop("Es wurden keine Bootstrapgewichte uebergeben! Moeglicherweise liegt es auch an den Variablennamen der Bootstrapgewichte, diese muessen die Namen 'gew1_1', 'gew1_2', 'gew1_3', ... haben.\n")
  }
  bwNum <- paste0(bw,"Num")
  date <- gsub("dat_","",names(xx))
  x <- copy(xx[[1]])
  y <- NULL
  if(!"gew1"%in%names(x)){
    stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
  }
  
  appendVarNumTmp(x, var, negativeZero)
  
  x[,gew1Num:=gew1*varNumTmp]
  x_bw <- copy(x)
  x[, (bw) := lapply(.SD, function(y){y * x_bw[['varNumTmp']]}), .SDcols = bw]
  setnames(x,old=bw,new=bwNum)
  
  if(length(xx)>1){
    date <- gsub("dat_","",names(xx)[1])
    datePrev <- gsub("dat_","",names(xx)[2])
    y <- copy(xx[[2]])
    if(!"gew1"%in%names(y)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
    
    appendVarNumTmp(y, var, negativeZero)
    
    y[,gew1Num:=gew1*varNumTmp]
    y_bw <- copy(y)
    y[, (bw) := lapply(.SD, function(z){z * y_bw[['varNumTmp']]}), .SDcols = bw]
    setnames(y,old=bw,new=bwNum)
  }
  num <- x[eval(parse(text=TFstring)),sum(gew1Num)]
  numb <- x[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bwNum]
  denum <- x[eval(parse(text=TFstring)),sum(gew1)]
  denumb <- x_bw[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bw]
  est <- num/denum
  estb <- numb/denumb
  sde <- sd(estb)
  if(is.null(y)){
    return(mzRComponent(date, est, estb, returnBR = replicates))
  }else{
    num2 <- y[eval(parse(text=TFstring)),sum(gew1Num)]
    numb2 <- y[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bwNum]
    denum2 <- y[eval(parse(text=TFstring)),sum(gew1)]
    denumb2 <- y_bw[eval(parse(text=TFstring)),lapply(.SD,sum),.SDcols=bw]
    est2 <- num2/denum2
    estb2 <- numb2/denumb2
    return(mzRComponent2(date, est, est2, estb, estb2, datePrev, returnBR = replicates))
  }
}
MedianX <- function(xx, TFstring,var, negativeZero = TRUE, replicates = replicates){
  gew1Num <- varNumTmp <- bwTmp <- gew1 <- bwNum <- NULL
  bw <- colnames(xx[[1]])[grepl("gew1_",colnames(xx[[1]]))]
  if(length(bw)==0){
    stop("Es wurden keine Bootstrapgewichte uebergeben! Moeglicherweise liegt es auch an den Variablennamen der Bootstrapgewichte, diese muessen die Namen 'gew1_1', 'gew1_2', 'gew1_3', ... haben.\n")
  }
  estb <- vector()
  date <- gsub("dat_","",names(xx))
  x <- copy(xx[[1]])
  y <- NULL
  if(!"gew1"%in%names(x)){
    stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
  }
  
  appendVarNumTmp(x, var, negativeZero)
  
  xtmp <- x[eval(parse(text=TFstring))]
  setkey(xtmp,varNumTmp)
  xtmp[,gew1Num:=cumsum(gew1)]
  est <- xtmp[gew1Num<=xtmp[,sum(gew1)/2],tail(varNumTmp,1)]
  #TODO: Schoener Loesung fuer die Generierung der Variablen
  for(i in seq_along(bw)){
    setnames(xtmp,bw[i],"bwTmp")
    estb[i] <- xtmp[ , laeken::weightedMedian(varNumTmp, bwTmp)]
    setnames(xtmp,"bwTmp",bw[i])
  }
  if(length(xx)>1){
    date <- gsub("dat_","",names(xx)[1])
    datePrev <- gsub("dat_","",names(xx)[2])
    y <- copy(xx[[2]])
    if(!"gew1"%in%names(y)){
      stop("Es wurden keine Gewichte uebergeben! Moeglicherweise liegt es auch am Variablennamen der Gewichte, diese muessen 'gew1' heissen.\n") 
    }
    
    estb2 <- vector()
    
    appendVarNumTmp(y, var, negativeZero)
    
    ytmp <-y[eval(parse(text=TFstring))]
    setkey(ytmp,varNumTmp)
    ytmp[,gew1Num:=cumsum(gew1)]
    est2 <- ytmp[gew1Num<=y[,sum(gew1)/2],tail(varNumTmp,1)]
    #TODO: Schoener Loesung fuer die Generierung der Variablen
    for(i in seq_along(bw)){
      setnames(ytmp,bw[i],"bwTmp")
      estb2[i] <- xtmp[ , laeken::weightedMedian(varNumTmp, bwTmp)]
      setnames(ytmp,"bwTmp",bw[i])
    }
  }
  sde <- sd(estb)
  if(is.null(y)){
    return(mzRComponent(date, est, estb, returnBR = replicates))
  }else{
    return(mzRComponent2(date, est, est2, estb, estb2, datePrev, returnBR = replicates))
  }
}
#' @export
#' @describeIn TotalMean Median. Robuste alternative zu \code{Mean}
Median <- function(x, TFstring = NULL, each = NULL, var, negativeZero = TRUE,
                   thousands_separator = TRUE, digits = 2, replicates = FALSE){
  ComputeNum(x = x, TFstring = TFstring, each = each, var = var, negativeZero = negativeZero,
             thousands_separator = thousands_separator, digits = digits, replicates = replicates,
             method = "MedianX")
}

#' @export
#' @describeIn TotalMean Arithmetisches Mittel
Mean <- function(x, TFstring = NULL, each = NULL, var, negativeZero = TRUE, 
                 thousands_separator = TRUE, digits = 2, replicates = FALSE){
  ComputeNum(x = x, TFstring = TFstring, each = each, var = var, negativeZero = negativeZero,
             thousands_separator = thousands_separator, digits = digits, replicates = replicates,
             method = "MeanX")
}


#' Schaetz- und Fehlerrechnungsfunktionen fuer numerische Variablen.
#' 
#' Funktion berechnet Totalwerte oder Mittelwerte einer numerischen Variable
#' und die zugehoerigen Fehler. 
#' 
#' Wiedergegeben wird der Schaetzer \code{est}, der Stichprobenfehler
#' \code{sd}, der Variationskoeffizient \code{cv} und die untere/obere Grenze
#' des 95\% Konfidenzintervalls \code{cil_2.5\%}/\code{ciu_97.5\%}.
#' Die Fehler werden mit Hilfe von Replikationsgewichten aus einem Bootstrapverfahren
#' berechnet, d.h. \code{sd} entspricht der Standardabweichung der mit den Bootstrapgewichten
#' berechneten Schaetzwerte, \code{cil_2.5\%} und \code{ciu_97.5\%} sind die 
#' entsprechenden 2.5\% und 97.5\% Quantile und \code{cv=sd/est}.
#' 
#' @aliases Total Mean Median
#' @param x MZ Daten - Output von Funktion \link{ImportData}.
#' @param TFstring Character oder NULL: Logische Einschraenkung der Gruppe. Falls NULL, gilt keine Einschraenkung.
#' @param each Character oder NULL: Name der Variable nach der getrennt berechnet werden soll. 
#' Hier koennen auch mehrere Variablen angegeben werden und zwar in einem character string, getrennt durch ein +, siehe Examples.
#' @param var character: Name der numerischen Variable oder Formel zur Berechnung dieser
#' z.b. 'x+y'.
#' @param negativeZero Logical: Wenn TRUE, werden die negativen Werte der numerischen
#' Variable auf 0 gesetzt.
#' @param thousands_separator Logical: Wenn TRUE, werden Tausendertrennzeichen
#' angezeigt.
#' @param digits Numerischer Wert: Anzahl der Nachkommastellen im angezeigten Ergebnis. Default
#' ist 2.
#' @param replicates Fürge einen Vektor aus Schätzwerten zum Output hinzu? Die Anzahl der Schätzwerte
#' pro Gruppe in `each` entspricht der Anzahl der Bootstrapreplikate (typischerweise 500). 
#' Siehe auch [getReplicates].
#' @return Output ist ein Objekt der Klasse \code{mzR}.
#' @seealso \code{\link{ImportData},\link{IndivImportData},\link{ImportAndMerge},
#' \link{GetLabels},\link{GroupSize},\link{GroupRate},\link{export.mzR}}
#' @rdname TotalMean
#' @examples
#' # Daten laden (oder Daten einlesen mit ImportData() bzw. IndivImportData())
#' data(mzTestData)
#' 
#' # Geleistete Arbeitsstunden: Absolutwerte
#' Total(mzTestData,TFstring="xerwstat==1&balt>=15&balt<=74",var="estund*13+dtstd*13")
#' # Geleistete Arbeitsstunden: Absolutwerte in Millionen
#' Total(mzTestData,TFstring="xerwstat==1&balt>=15&balt<=74",var="(estund+dtstd)*13/10^6")
#' 
#' #Durchschnittlich geleistete Arbeitsstunden
#' Mean(mzTestData,TFstring="xerwstat==1&balt>=15&balt<=74",var="estund+dtstd")
#' #Durchschnittlich geleistete Arbeitsstunden nach Geschlecht
#' Mean(mzTestData,TFstring="xerwstat==1&balt>=15&balt<=74",var="estund+dtstd", each="bsex")
#' #Durchschnittlich geleistete Arbeitsstunden nach Bundesland und Geschlecht
#' Mean(mzTestData,TFstring="xerwstat==1&balt>=15&balt<=74",var="estund+dtstd",each="xnuts2+bsex")
#' 
#' @export
Total <- function(x, TFstring = NULL, each = NULL, var, negativeZero = TRUE, 
                  thousands_separator = TRUE, digits = 2, replicates = FALSE){
  ComputeNum(x = x, TFstring = TFstring, each = each, var = var, negativeZero = negativeZero,
             thousands_separator = thousands_separator, digits = digits, replicates = replicates,
             method = "TotalX")
}
ComputeNum <- function(x, TFstring = NULL, each = NULL, var, negativeZero = TRUE,
                       thousands_separator = TRUE, digits = 2, replicates = FALSE, method){
  if(!method%in%c("MeanX","MedianX","TotalX"))
    stop("Unknown method for Numeric computation")
  if(is.null(TFstring))
    TFstring <- TRUE
  if(is.null(each)){
    res <- get(method)(x, TFstring, var = var, negativeZero = negativeZero, replicates = replicates)
    class(res) <- "mzR"
    attr(res,"each") <- NULL
    attr(res,"thousands_separator") <- thousands_separator
    attr(res,"digits") <- digits
    attr(res,"ergType") <- "Mean"
    attr(res,"TFstring") <- TFstring
    attr(res,"var") <- var
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
    eachvar <- each
  }
  for(l in x[[1]][,sort(unique(eval(parse(text=eachvar))))]){
    TFstringcur <- paste0(eachvar,"==",l,"&",TFstring)
    
    res[[paste0(eachvar,"_",l)]] <- get(method)(x, TFstringcur, var = var, 
                                                negativeZero = negativeZero, replicates = replicates)
    
  }
  class(res) <- "mzR"
  attr(res,"each") <- each
  attr(res,"thousands_separator") <- thousands_separator
  attr(res,"digits") <- digits
  attr(res,"ergType") <- "Mean"
  attr(res,"TFstring") <- TFstring
  attr(res,"var") <- var
  # if(any(is.na(unlist(res)))){
  #   warning("\nAchtung, moeglicherweise fehlen zu einer oder mehreren Beobachtungen Bootstrapgewichte!\n")
  # }    
  
  return(res)
}
