#' @importFrom graphics abline hist lines
#' @importFrom stats density
NULL

## Hilfsfunktionen zum erstellen der Listenelemente in mzR Objekten
mzRComponent <- function(date, est, estb, returnBR = FALSE) {
  sde <- sd(estb)
  out <- list(date = date, est = est, sd = sde, cv = sde/est, cil = quantNA(estb, .025),
       ciu = quantNA(estb, .975))
  
  ## ggf. bootstrap replikas hinzufügen
  if (returnBR)
    out$replicates <- as.numeric(estb)
  
  out
}
mzRComponent2 <- function(date, est, est2, estb, estb2, datePrev, returnBR = FALSE) {
  sde <- sd(estb)
  sde2 <- sd(estb2)
  absdiff <- est-est2
  absdiffb <- estb-estb2
  ratediff <- 100*(est-est2)/est2
  ratediffb <- 100*(estb-estb2)/estb2
  sdabs <- sd(absdiffb)
  sdrel <- sd(ratediffb)
  out <- list(
    date = date, est = est, sd = sde, cv = sde/est, cil = quantNA(estb, .025), 
    ciu = quantNA(estb, .975), datePrev = datePrev, estPrev = est2, sdPrev = sde2,
    cvPrev = sde2/est2, cilPrev = quantNA(estb2, .025), ciuPrev = quantNA(estb2, .975),
    absChange = absdiff, sdAbsChange = sdabs, cvAbsChange = sdabs/absdiff, 
    cilAbsChange = quantNA(absdiffb, .025), ciuAbsChange = quantNA(absdiffb, .975),
    relChange = ratediff, sdRelChange = sdrel, cvRelChange = sdrel/ratediff, 
    cilRelChange = quantNA(ratediffb,.025), ciuRelChange = quantNA(ratediffb, .975)
  )
  
  ## ggf. bootstrap replikas hinzufügen
  if (returnBR) {
    out$replicates <- as.numeric(estb)
    out$replicatesPrev <- as.numeric(estb2)
  }
  
  out
}

# Statt "Ver\u00E4nderung" doch besser "Change" in Anzeige
CompFehlerX <- function(x,each=NULL,thousands_separator=TRUE,digits=2){  
  if(is.null(each)){
    nname <- nPrevname <- NULL
    if("n"%in%names(x)){
      nname <- "n"
    }
    if("nPrev"%in%names(x)){
      nPrevname <- "nPrev"
    }
    if(any(grepl("Prev",names(x)))){
      print_obj <- as.data.frame(rbind(
        unlist(x[c("est","sd", "cv", "cil","ciu", nname)] ) ,
        unlist(x[c("estPrev","sdPrev", "cvPrev", "cilPrev","ciuPrev", nPrevname)]),
        c(unlist(x[c("absChange","sdAbsChange","cvAbsChange","cilAbsChange","ciuAbsChange")]),rep(NA, length(nPrevname))),
        c(unlist(x[c("relChange","sdRelChange","cvRelChange","cilRelChange","ciuRelChange")]),rep(NA, length(nPrevname)))
      ))
      print_obj <- round(print_obj,digits=digits)
      
      if(thousands_separator){
        print_obj <- format(print_obj, big.mark = ",")
      }
      row.names(print_obj) <- c(x[["date"]],x[["datePrev"]],"Absolute change","Relative change")
      colnames(print_obj)[1:5] <- c("est","sd", "cv",paste(names(x[c("cil","ciu")]),unlist(lapply(x[c("cil","ciu")],names)),sep="_"))
    }else{
      print_obj <- as.data.frame(x[c("est","sd", "cv", "cil","ciu", nname)])
      print_obj <- round(print_obj,digits=digits)
      if(thousands_separator){
        print_obj <- format(print_obj, big.mark = ",")
      }
      row.names(print_obj) <- x[["date"]]
      colnames(print_obj)[1:5] <- c("est","sd", "cv",paste(names(x[c("cil","ciu")]),unlist(lapply(x[c("cil","ciu")],names)),sep="_"))
    }
    
    print(print_obj)  
    
  }else{
    nname <- nPrevname <- NULL
    if("n"%in%names(x[[1]])){
      nname <- "n"
    }
    if("nPrev"%in%names(x[[1]])){
      nPrevname <- "nPrev"
    }
    if(any(grepl("Prev",names(x[[1]])))){
      print_obj_list <- list()
      for(i in 1:length(x)){
      print_obj <- as.data.frame(rbind(
        unlist(x[[i]][c("est","sd", "cv", "cil","ciu",nname)]) ,
        unlist(x[[i]][c("estPrev","sdPrev", "cvPrev", "cilPrev","ciuPrev",nPrevname)]),
        c(unlist(x[[i]][c("absChange","sdAbsChange","cvAbsChange","cilAbsChange","ciuAbsChange")]), if(!is.null(nname)){NA}),
        c(unlist(x[[i]][c("relChange","sdRelChange","cvRelChange","cilRelChange","ciuRelChange")]), if(!is.null(nname)){NA})
      ))
      print_obj <- round(print_obj,digits=digits)
      if(thousands_separator){
        print_obj <- format(print_obj, big.mark = ",")
      }
      row.names(print_obj) <- c(x[[i]][["date"]],x[[i]][["datePrev"]],"Absolute change","Relative change")
      colnames(print_obj)[1:5] <- c("est","sd", "cv",paste(names(x[[i]][c("cil","ciu")]),unlist(lapply(x[[i]][c("cil","ciu")],names)),sep="_"))
      print_obj_list[[length(print_obj_list)+1]] <- print_obj
      names(print_obj_list)[i] <- names(x)[i]
      
      }
      #do.call(rbind,rbind(print_obj_list,rep("",5)))      
      }else{      
        print_obj_list <- list()
        for(i in 1:length(x)){
          print_obj<- as.data.frame(x[[i]][c("est","sd", "cv", "cil","ciu",nname)])
          print_obj <- round(print_obj,digits=digits)
          if(thousands_separator){
            print_obj <- format(print_obj, big.mark = ",")
          }
          row.names(print_obj) <- x[[i]][["date"]]
          colnames(print_obj)[1:5] <- c("est","sd", "cv",paste(names(x[[i]][c("cil","ciu")]),unlist(lapply(x[[i]][c("cil","ciu")],names)),sep="_"))
          print_obj_list[[length(print_obj_list)+1]] <- print_obj
          names(print_obj_list)[i] <- names(x)[i]
      }
#       cat(x[[1]][["date"]])
#       do.call(rbind,print_obj_list)
      }
  print(print_obj_list)

  }
    
}


#' Ergebnisse der Schaetz-und Fehlerrechnungsfunktionen werden in abgewandelter
#' Form in der R-Konsole ausgegeben.
#' 
#' Funktion liest Ergebnisse von \code{GroupSize} oder \code{GroupRate} ein und
#' gibt sie in der R-Konsole in etwas abgewandelter Form aus bzw. als Objekt
#' zurueck.
#' 
#' ...
#' 
#' @param x Ergebnis von GroupSize, GroupRate, Total oder Mean.
#' @param ... currently ignored
#' @return Falls \code{each} nicht auf \code{NULL} gesetzt wird, wird eine
#' Liste mit den Ergebnissen ausgegeben. Deren Laenge variiert je nach
#' Dimension der \code{each}-Variable. Ist \code{each=NULL}, so wird ein
#' Dataframe ausgegeben.
#' @rdname print
#' @method print mzR
#' @export
#' 
print.mzR <-function(x,...){
  CompFehlerX(x,attr(x,"each"),attr(x,"thousands_separator"),attr(x,"digits"))
}

#' Extrahiere die Bootstrap Replikla aus einem mzR Objekt
#' 
#' Falls ein `mzR` Objekt mit dem Parameter `replicate = TRUE` erzeugt wurde, lassen sich mit dieser
#' Funktion alle Schätzwerte zurückgeben.
#' 
#' @param x Ein Objekt der Klasse `mzR`. Typischerweise generiert durch `GroupRate`, `Groupsize`,
#' `Median`, `Mean` oder `Total`.
#' @export
#' @return Tabelle mit Bootstrapreplika. Die Spalten entsprechen den Gruppierungsvariablen der
#' Auswertung, falls vorhanden (siehe das Argument `each` in `GroupRate`, `Median`, etc.). Die Zeilen
#' enstsprechen den Bootstrapgewichten (typischerweise 500 Stück).
#' @examples 
#' library(dplyr)  ## Für %>%
#' library(ggplot2)
#' 
#' ######################## Beispiel 1: Durschnittliche Arbeitsstunden #############################
#' 
#' dat <- ImportData(year = 2014, quarter = 4)
#' mzObj <- Mean(dat, TFstring = "xerwstat==1&balt >= 15&balt <= 74", 
#'               var = "estund*13+dtstd*13", replicates = TRUE)
#' replicates <- getReplicates(mzObj)$replicates
#' 
#' hist(replicates, main = "Durschnittlich geleistete Arbeitsstunden, 95% KI", freq = FALSE,
#'      col = "lightblue", xlab = "Durchschnittliche Wochenstunden in Replika")
#' abline(v = quantile(replicates, c(.025, 0.975)), col = "red", lwd = 2)
#' lines(density(replicates), col = "darkblue", lwd = 2)
#' 
#' ########################### Beispiel 2: Wohnkosten nach Geschlecht ##############################
#' 
#' mzObj2 <- Mean(dat, TFstring = "xerwstat==1&balt >= 15&balt <= 74", var = "wkges", 
#'                replicates = TRUE, each = "bsex")
#' getReplicates(mzObj2) %>% tidyr::gather(Geschlecht, wk) %>% 
#'   mutate(Geschlecht = recode(Geschlecht, bsex_1 = "M", bsex_2 = "W")) %>%
#'   ggplot(aes(Geschlecht, wk, fill = Geschlecht)) + 
#'   geom_boxplot(outlier.shape = NA, alpha = 0.5) +
#'   geom_jitter(aes(col = Geschlecht), alpha = 0.5) +
#'   ylab("Durchschnittliche Wohnkosten in Replika")
#'   
#' ###################### Beispiel 3: Arbeitslosenquote nach Bundesland ############################
#' 
#' mzObj3 <- GroupSize(dat,TFstring="xerwstat==2&balt>=15&balt<=74", replicates = TRUE, 
#'                     each = "xnuts2")
#' getReplicates(mzObj3) %>% tidyr::gather(Bundesland, unemployment) %>% 
#' ggplot(aes(Bundesland, unemployment, fill = Bundesland)) + 
#'   geom_boxplot(outlier.shape = NA, alpha = 0.5) +
#'   geom_jitter(aes(col = Bundesland), alpha = 0.05) +
#'   ylab("Arbeislosenzahlen in Bootstrap-Replika")
getReplicates <- function(x) {
  if(is.null(attr(x, "each")))
    as.data.table(list(replicates = x$replicates))
  else
  lapply(x, function(comp) comp$replicates) %>% as.data.table
}

#' Visualisiere ein mzR Objekt
#' 
#' Standarplots für `mzR` Objecte. Nur anwendbar, wenn das Objekt bootsrtap Replikate beinhaltet,
#' ansonsten wird eine Warnung geworfen. Siehe auch [getReplicates].
#' @param x Ein Objekt der Klasse `mzR`.
#' @param ... Ungenützt.
#' @examples 
#' dat <- ImportData(year = 2014, quarter = 4)
#' mzObj <- Mean(dat, TFstring = "xerwstat==1&balt >= 15&balt <= 74", 
#'               var = "estund*13+dtstd*13", replicates = TRUE)
#' plot(mzObj)
#' @import ggplot2
#' @export
plot.mzR <- function(x, ...) {
  value <- NULL
  rep <- getReplicates(x)
  if (nrow(rep) == 0) {
    warning("no bootstrap replicates available")
    return(invisible(NULL))
  }
  if (ncol(rep) == 1) {
    rep <- rep[[1]]
    plot_text <- paste("Histogram of", attr(x, "var"), "(replicates)")
    hist(rep, freq = FALSE, col = "lightblue", main =plot_text, xlab = attr(x, "var"))
    abline(v = quantile(rep, c(0.025, 0.975)), col = "red", lwd = 2)
    lines(density(rep), col = "darkblue", lwd = 2)
    return(invisible(NULL))
  }
  alph <- 1/ncol(rep)
  rep %>% tidyr::gather() %>% 
    ggplot(aes(key, value, fill = key)) + 
    geom_boxplot(outlier.shape = NA, alpha = alph) +
    geom_jitter(aes(col = key), alpha = alph) +
    ylab(paste(attr(x, "var"), "(replicates)")) +
    xlab(attr(x, "each")) +
    scale_color_discrete(name = xlab(attr(x, "each"))) +
    scale_fill_discrete(guide=FALSE)
    
}

#' Kopnvertiere ein `mzR` Objekt in ein Tabelle
#'
#' Generische Funktion [as.data.frame] implementiert für `mzR` Objekte.
#' 
#' @param x Ein Objekt der Klasse `mzR`.
#' @param ... Ungenützt
#' @export
as.data.frame.mzR <- function(x, ...) {
  if ("cil" %in% names(x))
    x <- list(noeachvar = x)
  dat <- data.frame(
    est = lapply(x, function(x) x$est) %>% as.numeric,
    sd = lapply(x, function(x) x$sd) %>% as.numeric,
    cv = lapply(x, function(x) x$cv) %>% as.numeric,
    cil = lapply(x, function(x) x$cil) %>% as.numeric,
    ciu = lapply(x, function(x) x$ciu) %>% as.numeric
  )
  if("n"%in%names(x[[1]])){
    dat$n <- lapply(x, function(x)x$n) %>% as.numeric
  }  
  if("noeachvar" %in% names(x))
    return(dat)
  nms <- names(x)
  indVar <- lapply(nms,function(x)gregexpr("_",x)[[1]])
  # fix problem with variables that contain one or multiple _
  if(any(sapply(indVar,length)>1)){
    for(i in seq_along(nms)){
      pos <- tail(indVar[[i]],1)
      nms[i] <- paste0(substring(nms[i],1,pos-1),"§",substring(nms[i],pos+1))
    }
    nms <- strsplit(nms, "§")
  }else{
    nms <- strsplit(nms, "_")
  }
  nms1 <- nms[[1]]
  factors <- nms1[1:(length(nms1)-1)]
  id <- lapply(nms, function(el){ el[[length(el)]]}) %>% as.character()
  
  vals <- NULL
  for(i in 1:length(id)) {
    vals[[i]] <- substring(id[i], rev(pmax(seq(nchar(id[i]) - 2, -1, -3))), rev(seq(nchar(id[i]), 1, -3)))
  }
  vals <- as.data.frame(t(as.data.frame(as.list(vals))))
  names(vals) <- factors
  rownames(vals) <- NULL
  
  cbind(vals, dat)
}
