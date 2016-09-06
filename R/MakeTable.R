#############################################################################
###                        --- Functions ---                              ###
#############################################################################

# Combine Tables als Funktion in Paket aufnehmen oder lieber herzeigen wie do.call(rbind,tables) geht...?
# CombineTables <- function(tables){
#   
#   if(!is.null(names(tables))){
#     for(i in 1:length(tables)){
#       if(names(tables)[i]!="")
#         rownames(tables[[i]])[1] <- names(tables)[i]
#     }
#   }
#   do.call(rbind,tables)
# }

parameterSpellCheck <- function(x){
  y <- unlist(x)
  p <- c("TFstring","TFstring2","each","fun","var","Total","Mean","GroupSize","GroupRate","scaleF")
  
  if(!all(sapply(p,function(q) identical(grep(q,names(y),ignore.case=FALSE),grep(q,names(y),ignore.case=TRUE))))){
    
    warning("\n\nFailed Spell-Check! \nEin Parameter wurde moeglicherweise falsch geschrieben (Achtung: Case Sensitivity), das kann zu einem Abbruch der Funktion fuehren.\n",
            "\nBitte ueberpruefe Parameter '",paste0(p[!sapply(p,function(q) identical(grep(q,names(y),ignore.case=FALSE),grep(q,names(y),ignore.case=TRUE)))],collapse="' und '"),"'\n")
  }
}
makeEachVar <- function(x){
  x <- x[length(x):1]
  res <- 0
  for(i in 1:length(x)){
    res <- res + x[i]*10^((i-1)*3)
  }
  return(res)
}

# estimator: est,estPrev,absChange,relChange



#' Funktion zur Erstellung von Tabellen in Matrix-Form.
#' 
#' Erstellt eine Tabelle in Matrix-Form.
#' 
#' Die Syntax fuer Spalten, Zeilen und Block Listen \code{col} ist eine aus
#' Sublisten bestehende Liste. Jede Subliste steht fuer eine eigene Spalte der
#' zu erstellenden Tabelle in Matrix-Form und gibt an, was fuer die
#' entsprechende Spalte berechnet werden soll. Jede Subliste kann die Argumente
#' \code{fun}, \code{TFstring}, \code{TFstring2}, \code{digits}, \code{var} und
#' \code{scaleF} enthalten. \code{fun} muss dabei gesetzt werden und zwar auf
#' eine der vier Funktionen \code{GroupSize}, \code{GroupRate}, \code{Mean} und
#' \code{Total} aus dem mzR-Paket, die restlichen Parameter sind optional.
#' \code{TFstring}, \code{TFstring2}, \code{digits} und \code{var} sind einfach
#' die Parameter aus eben genannten Funktionen, \code{scaleF} ist der
#' Skalierungsfaktor der auf die jeweilige Spalte angewendet werden soll, also
#' z.B. \code{scaleF="/1000"} gibt an, dass die Ergebnisse der entsprechenden
#' Spalte durch 1000 dividiert werden sollen, er ist als character zu
#' uebergeben.
#' 
#' \code{row} ist eine 'list', 'named list' oder 'partly named list' die
#' Sublisten enthalten kann aber nicht muss. Sublisten werden eigentlich nur
#' uebergeben, wenn einem Element aus \code{row} mehr als ein Parameter
#' zugewiesen werden soll. Zusaetzlich zu den moeglichen Argumenten fuer
#' \code{col} (ausser \code{digits}) kann hier auch noch \code{each} gesetzt
#' werden (auch ein Parameter aus den mzR-Schaetz-und
#' Fehlerrechnungsfunktionen), \code{fun} ist hier prinzipiell optional, muss 
#' aber in vielen Faellen fuer einen sinnvollen Output angegeben werden. 
#' Will man beispielsweise einen Mittelwert berechnen, so wuerde das dementsprechende 
#' \code{row} Sublistenelement z.B. folgendermassen aussehen: 
#' \code{list(fun="Mean",var="VarName",TFstring="!is.na(VarName)",scaleF="*1")},
#' d.h. es wird in der entsprechenden Zeile der Tabelle jeweils der Mittelwert
#' der Variable VarName berechnet, zusaetzlich dazu werden aber ueber den \code{TFstring} 
#' auch noch die fehlenden Werte in der Variable VarName ausgeschlossen.
#' Wird der Parameter
#' \code{scaleF} bei \code{row} gesetzt, so hat er den Vorzug vor \code{scaleF}
#' bei \code{col} - sollte er dort gesetzt worden sein (hier nicht vergessen,
#' falls in \code{row} Raten berechnet werden sollen \code{scaleF="*1"}
#' setzen). Falls \code{GroupRate}, \code{Mean} oder \code{Total} sowohl in 
#' \code{row} als auch \code{col} gesetzt wurden wird fuer die jeweilige Zelle 
#' im Output kein Ergebnis ausgegeben.
#' 
#' \code{block} ist eine Liste, die gewisse Einschraenkungen (oder \code{NULL},
#' falls keine Einschraenkung gemacht werden soll) enthaelt, welche jeweils
#' fuer alle Zeilen ausgefuehrt werden sollen. Diese Einschraenkungen werden
#' pro \code{block}-Element fuer alle \code{row}-Elemente ausgefuehrt. D.h. es
#' kann z.B. eine Output-Tabelle erstellt werden die im ersten 'Block' die
#' Ergebnisse fuer Schnecken und im zweiten Block die Ergebnisse fuer
#' Nicht-Schnecken enthaelt.
#' 
#' Kleine Bemerkung am Rande: Fuer die meisten Publikationen ist es sinnvoll, 
#' keine \code{digits} bei \code{col} bei \code{MakeTable()} zu setzen da oft die vorgegebenen
#' Excel-Formate das Runden uebernehmen.
#' 
#' @param dat MZ Daten - Output von Funktion \link{ImportData}.
#' @param col Listenobjekt um Spalten zu definieren, siehe Details.
#' @param row Listenobjekt oder NULL um Zeilen zu definieren, siehe Details.
#' @param block Listenobjekt oder NULL um Block-Output definieren, siehe
#' Details.
#' @param estimator character um festzulegen, welcher Schaetzer in die Tabelle
#' kommen soll. Auwahlmoeglichkeiten sind \code{"est"}, \code{"estPrev"},
#' \code{"absChange"} und \code{"relChange"}.
#' @param error character um festzulegen, welcher Genauigkeitsschaetzer zur Markierung von Zellen verwendet werden soll. 
#' Auswahlmoeglichkeiten sind \code{"cv"} (Variationskoeffizient) und \code{"ci"} (Konfidenzintervall). 
#' Sollte \code{error="cv"} gewaehlt werden, so sind die Parameter \code{lim1} bzw. \code{lim2} gegebenenfalls zu spezifizieren. 
#' Falls jedoch \code{error="ci"} gewaehlt wird, so sind diese Limits nicht notwendig da in diesem Fall geschaut wird, ob der jeweilige 
#' Schaetzer \code{estimator} innerhalb des Konfidenzintervalls liegt. Man kann jedoch \code{markLeft1}, \code{markRight1} und \code{markValue1}
#' spezifizieren falls die signifikanten Zellen anders als durch die Defaulteinstellung 
#' \code{markLeft1 = "("}, \code{markRight1 = ")"} und \code{markValue1 = NULL} markiert werden sollen.
#' @param lim1 numerischer Wert: falls \code{lim1}>\code{error}, wird der
#' entsprechende Wert von \code{estimator} in der Tabelle durch
#' \code{markLeft1}, \code{markValue1} und \code{markRight1} ersetzt. Ist nur relevant falls code{error="cv"}.
#' @param markValue1 character oder NULL: falls NULL, wird der jeweilige Wert
#' von \code{estimator} nicht ueberschrieben.
#' @param markLeft1 character: wird links zu \code{markValue1} hinzugefuegt.
#' @param markRight1 character: wird rechts zu \code{markValue1} hinzugefuegt.
#' @param lim2 numerischer Wert: falls \code{lim2}>\code{error}, wird der
#' entsprechende Wert von \code{estimator} in der Tabelle durch
#' \code{markLeft2}, \code{markValue2} und \code{markRight2} ersetzt. Ist nur relevant falls code{error="cv"}.
#' @param markValue2 character oder NULL: falls NULL, wird der jeweilige Wert
#' von \code{estimator} nicht ueberschrieben. Ist nur relevant falls code{error="cv"}.
#' @param markLeft2 character: wird links zu \code{markValue2} hinzugefuegt. Ist nur relevant falls code{error="cv"}.
#' @param markRight2 character: wird rechts zu \code{markValue2} hinzugefuegt. Ist nur relevant falls code{error="cv"}.
#' @param rowPriority TRUE/FALSE ob bei der Berechnung von Raten die Zeilenlogik vor der Spaltenlogik gelten soll.
#' @param returnCommands TRUE/FALSE ob statt einer Tabelle die Befehle
#' ausgegeben werden sollen die zur Erstellung der Tabelle ausgefuehrt werden
#' wuerden.
#' @return Output ist eine Tabelle in Matrix-Form mit durch \code{col},
#' \code{row} und \code{block} definierten Spalten und Zeilen.
#' @seealso
#' \code{\link{FillExcelTemplate},\link{MakeAKETimeInstantsTable},\link{ImportData},\link{IndivImportData}}
#' @export
#' @examples
#' \dontrun{
#' dat <- ImportData(year=2014,quarter=4)
#' ### Spalten definieren
#' col <- list()
#' col[[length(col)+1]] <- list(fun="GroupSize",TFstring="balt>=15&balt<=74",
#'   digits=0, scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupSize",TFstring="balt>=15&balt<=25",
#'   digits=0, scaleF="/1000")
#' col[[length(col)+1]] <- list(fun="GroupRate",TFstring="balt>=15&balt<=25", 
#'   TFstring2="balt>=15&balt<=74",digits=1)
#' col[[length(col)+1]] <- list(fun="GroupRate",
#'   TFstring="xerwstat==1&balt>=15&balt<=74", 
#'   TFstring2="xerwstat%in%c(1,2)&balt>=15&balt<=74",digits=1)
#' col[[length(col)+1]] <- list(fun="Total",
#'   TFstring="xerwstat==1&balt>=15&balt<=74",var="dtstd",
#'   digits=2, scaleF="/24/365")
#' ### Zeilen definieren
#' row <- list(
#'   NULL,
#'   each="xnuts2",
#'   list(TFstring="bpras!=1 & balt>=15 & balt<=64",scaleF="*1"),
#'   each="bsex+xnuts2",
#'   TFstring="balt>=15&balt<=25"
#' )
#' 
#' ### Bloecke definieren
#' block <- list(NULL, "bsex==1", "bsex==2")
#' 
#' ### Erstellen 2 Tabellen fuer FillExcelTemplate(), einmal mit und einmal ohne Limits.  
#' ### Tabelle mit Limits (Variationskoeffizient)
#' tab1 <- MakeTable(dat,col=col,row=row, block=block,error="cv", 
#'   lim1=0.17,lim2=0.25)
#' ### Tabelle ohne Limits (Variationscoeffizient)
#' tab2 <- MakeTable(dat,col=col,row=row, block=block,error="cv") 
#' 
#' ### Commands ansehen fuer tab1
#' tab1_commands <- MakeTable(dat,col=col,row=row, block=block,error="cv", 
#'   lim1=0.17,lim2=0.25,returnCommands=TRUE)
#' ## -> Bsp: block 1, col 3, row 4: 
#' tab1_commands[[1]][[3]][[4]]
#' 
#' }
#' 
MakeTable  <- function(dat,col,row=NULL,block=NULL,estimator="est",error="cv",
                       lim1=Inf,markLeft1="(",markRight1=")",markValue1=NULL,
                       lim2=Inf,markLeft2="(",markRight2=")",markValue2="x",
                       rowPriority=TRUE, returnCommands=FALSE){
  #emptyRows=FALSE, 
  parameterSpellCheck(row)
  parameterSpellCheck(col)
  
  # Bei Veraenderungen wollen wir ein weniger strenges Mass als den Variationskoeffizienten.
  # Es wird also nur geschaut, ob der geschaetzte Wert innerhalb des Konfidenzintervalls liegt.
  if(error=="ci"){
    error <- "cil"
    lim1 <- 1
    lim2 <- Inf
  }
  if(estimator=="absChange"){
    error <- paste0(error,toupper(substr(estimator,1,1)), substr(estimator,start=2,stop=nchar(estimator)))
  }else if(estimator=="relChange"){
    error <- paste0(error,toupper(substr(estimator,1,1)), substr(estimator,start=2,stop=nchar(estimator)))
  } else if(estimator=="estPrev"){
    error <- paste0(error,"Prev")
  }
  if(any(unlist(lapply(col,function(x)is.null(x$fun)))))
    stop("col in MakeTable() muss immer einen Parameter 'fun' enthalten!")
  
  if(!is.null(block)){
    save.dat <- copy(dat)  
    outlist <- list()
    outcmd_list <- list()
  }
  
  leBlock <- ifelse(length(block)>0,length(block),1)
  
  ##eigentlich brauch ich diese each-Aufspragelung nur wenn GroupRates oder sowas berechnet werden sollen...
  if((any(grepl("each",row)) || any(grepl("each",names(row))))){# && rowPriority){
    if(returnCommands){
      warnings("\nAchtung, 'each' aus 'row' wird aufgesplittet in mehrere TFstrings. Bei returnCommands-Output beachten!\n")
    }
    while(any(grepl("each",row))){
      eachind <- grep("each",row)[1] #row wird ja ueberschrieben
      each <- row[[eachind]]$each
      eachrow_orig <- row[[eachind]]
      ##############
      if(length(grep("\\+",each))>0){
        eachv <- strsplit(each,"\\+")[[1]]
        eachvar <- paste(eachv,collapse="_")
        for(i in 1:length(dat)){
          dat[[i]][[eachvar]] <- apply(dat[[i]][,eachv,with=FALSE],1,makeEachVar)
        }
      }else{
        eachv <- eachvar <- each
      }
      TFstringeach <- vector()
      for(l in dat[[1]][,sort(unique(eval(parse(text=eachvar))))]){
        TFstringeach[length(TFstringeach)+1] <- paste0(eachvar,"==",l)
      }  
      #############
      eachlist <- list()
      for(e in 1:length(TFstringeach)){
        eachlist[[length(eachlist)+1]] <- eachrow_orig
        eachlist[[e]][["each"]] <- NULL 
        if(!is.null(eachlist[[e]][["TFstring"]])){
          eachlist[[e]][["TFstring"]] <- paste0(c(eachlist[[e]][["TFstring"]],TFstringeach[e]),collapse=" & ")
        }else{
          eachlist[[e]][["TFstring"]] <- TFstringeach[e]  
        }
      }
      row[[eachind]]<-NULL
      row <- append(row,eachlist,after=eachind-1)
      
    }
    while(any(grepl("each",names(row)))){
      eachind <- grep("each",names(row))[1] #row wird ja ueberschrieben
      each <- row[[eachind]]
      ##############
      if(length(grep("\\+",each))>0){
        eachv <- strsplit(each,"\\+")[[1]]
        eachvar <- paste(eachv,collapse="_")
        for(i in 1:length(dat)){
          dat[[i]][[eachvar]] <- apply(dat[[i]][,eachv,with=FALSE],1,makeEachVar)
        }
      }else{
        eachv <- eachvar <- each
      }
      TFstringeach <- vector()
      for(l in dat[[1]][,sort(unique(eval(parse(text=eachvar))))]){
        TFstringeach[length(TFstringeach)+1] <- paste0(eachvar,"==",l)
      }  
      #############
      eachlist <- list()
      for(e in 1:length(TFstringeach)){
        eachlist[[length(eachlist)+1]] <- TFstringeach[e]
        names(eachlist)[length(eachlist)] <- "TFstring"
      }
      row[[eachind]]<-NULL
      row <- append(row,eachlist,after=eachind-1)
    }
  }
  
  for(i in 1:leBlock){
    
    if(!is.null(block[[i]])){
      dat <- lapply(dat,function(x)x[eval(parse(text=block[[i]])),])
    }
    
    out <- lapply(col,function(colx){
      
      if(returnCommands){
        cmd_list <- list()
      }
      out <- list()
      novaluelist <- list()
      
      colcommands <- colx[-which(names(colx)%in%c("fun","digits","scaleF"))]
      if(length(colcommands)==0){
        colcommands <- list(TFstring=NULL)
      }
      origcolcommands <-  colcommands
      
      whichcol <- which(sapply(col,function(z)isTRUE(all.equal(colx,z))))
      
      nrow <- length(row)
      if(nrow==0)
        nrow <- 1
      
      colx.orig <- colx
      row.orig <- row
      
      ###vielleicht fuer showCommands so eine Art j_orig einfuehren....?
      
      for(j in 1:nrow){
        
        ## Alle row-commands auf ein Format bringen, also in Listenform
        if(!is.list(row[[j]]) && !is.null(row[[j]])){
          rowcommands <- eval(parse(text=paste0("list(",names(row)[j],"=\"",row[[j]],"\")")))
        }else if(is.null(row[[j]])){
          rowcommands  <- list(NULL) 
        }else{
          rowcommands <- row[[j]]
        }
        cellcommands <- list()
        
        if(is.null(unlist(rowcommands))){##hier einfach als TFstring=NULL herrichten
          names(rowcommands) <- "TFstring"
        }
        ## zuerst args fertig herrichten, dann erst zu commands machen
        #if(!is.null(unlist(rowcommands))){#else Fall: args bleiben args, ausser bei rowPriority=TRUE
        
        ## Einige Plaus-Schritte
        if("TFstring2"%in%names(rowcommands)){
          row[[j]][["fun"]] <- "GroupRate"##evt hier noch warning einbauen fall orig row fun nicht GroupRate war sondern was andres
          rowcommands[[length(rowcommands)+1]] <- "GroupRate"
          names(rowcommands)[length(rowcommands)] <- "fun"
        }
        if("TFstring"%in%names(rowcommands) && !"var"%in%names(rowcommands) && !"TFstring2"%in%names(rowcommands) && !names(rowcommands)%in%c("fun")){
          row[[j]][["fun"]] <- "GroupSize" #row Element NULL wird nicht hier behandelt sondern ueberhaupt extra...wahrscheinlich
          rowcommands[[length(rowcommands)+1]] <- "GroupSize"
          names(rowcommands)[length(rowcommands)] <- "fun"
        }
        # if("each"%in%names(rowcommands) && !"var"%in%names(rowcommands) && !"TFstring2"%in%names(rowcommands) && !names(rowcommands)%in%c("fun")){
        #   row[[j]][["fun"]] <- "GroupSize" #row Element NULL wird nicht hier behandelt sondern ueberhaupt extra...wahrscheinlich
        #   rowcommands[[length(rowcommands)+1]] <- "GroupSize"
        #   names(rowcommands)[length(rowcommands)] <- "fun"
        # }
        
        ## Funktion in row auf Funktion aus col setzen falls keine uebergeben wird und obiges nicht greift (kann das ueberhaupt noch vorkommen?)
        ## falls nein, kann man rowcommands zuweisungen in den obigen Schritten wieder loeschen
        if(all(!names(rowcommands)%in%c("fun"))){
          row[[j]][["fun"]] <- colx[["fun"]]             
        }
        ## rowcommands auf zulaessige Parameter einschraenken
        if(any(!names(rowcommands)%in%c("TFstring","TFstring2","var"))){  # ,"each"))){
          rowcommands <- rowcommands[-which(!names(rowcommands)%in%c("TFstring","TFstring2","var"))] # ,"each"))]
        }  
        
        ## novaluelist befuellen (warnings fehlen noch)
        if((row[[j]][["fun"]]%in%c("GroupRate","Total","Mean") && colx[["fun"]]%in%c("GroupRate","Total","Mean")) || (colx[["fun"]]%in%c("GroupRate","Total","Mean") && isTRUE(row[[j]][["fun"]]%in%c("GroupRate","Total","Mean")))){
          novaluelist[[length(novaluelist)+1]] <- c(row=j,col=whichcol)
        }
        
        # else if("var"%in%names(rowcommands) && !row[[j]][["fun"]]%in%c("Total","Mean")){#bei col MUSS ja Fkt uebergeben werden
        #   warning("var ",rowcommands[["var"]]," wird bei 'row ",j,", col ",whichcol, "' ignoriert da Funktion ",row[[j]][["fun"]]," ausgefuehrt wird. \n")
        # }## sollte schon dadurch abgefangen werden, dass bei fehlender fun einfach die Spaltenfun zugewiesen wird und somit kein Wert ausgegeben wird da dann z.B. MeanxMean
        
        ### Total und Mean-Zeug spaeter genauer machen 
        
        ## Nur relevant fuer GroupRate
        #GroupRate geht nur mit GroupSize!  
        
        if(colx[["fun"]]=="GroupRate" && row[[j]][["fun"]]=="GroupSize"){       
          if(rowPriority){
            cellcommands[[length(cellcommands)+1]] <- paste0(c(rowcommands[["TFstring"]],colcommands[["TFstring"]]),collapse=" & ")
            names(cellcommands)[length(cellcommands)] <- "TFstring"
            
            cellcommands[[length(cellcommands)+1]] <- paste0(c(rowcommands[["TFstring"]],colcommands[["TFstring2"]]),collapse=" & ")
            names(cellcommands)[length(cellcommands)] <- "TFstring2"
          }else{
            cellcommands[[length(cellcommands)+1]] <- paste0(c(rowcommands[["TFstring"]],colcommands[["TFstring"]]),collapse=" & ")#,colcommands[["TFstring2"]]#braucht man eh nicht, sollte eigentlich in rowcommands[["TFstring"]] drinstecken
            names(cellcommands)[length(cellcommands)] <- "TFstring"
            cellcommands[[length(cellcommands)+1]] <- paste0(c(colcommands[["TFstring"]]),collapse=" & ")#,colcommands[["TFstring2"]]
            names(cellcommands)[length(cellcommands)] <- "TFstring2"
          }
          cellcommands[[length(cellcommands)+1]] <- "GroupRate"
          names(cellcommands)[length(cellcommands)] <- "fun"
        }else if(colx[["fun"]]=="GroupSize" && row[[j]][["fun"]]=="GroupRate"){
          if(rowPriority){
            cellcommands[[length(cellcommands)+1]] <- paste0(c(rowcommands[["TFstring"]],colcommands[["TFstring"]]),collapse=" & ")
            names(cellcommands)[length(cellcommands)] <- "TFstring"
            
            cellcommands[[length(cellcommands)+1]] <- paste0(c(rowcommands[["TFstring"]],rowcommands[["TFstring2"]]),collapse=" & ")
            names(cellcommands)[length(cellcommands)] <- "TFstring2"
          }else{
            cellcommands[[length(cellcommands)+1]] <- paste0(c(rowcommands[["TFstring"]]),colcommands[["TFstring"]],collapse=" & ")#,rowcommands[["TFstring2"]]#braucht man eh nicht, sollte eigentlich in colcommands[["TFstring"]] drinstecken
            names(cellcommands)[length(cellcommands)] <- "TFstring"
            cellcommands[[length(cellcommands)+1]] <- paste0(c(colcommands[["TFstring"]]),collapse=" & ")#,rowcommands[["TFstring2"]]
            names(cellcommands)[length(cellcommands)] <- "TFstring2"
          }
          cellcommands[[length(cellcommands)+1]] <- "GroupRate"
          names(cellcommands)[length(cellcommands)] <- "fun"
        }else if(row[[j]][["fun"]]!=colx[["fun"]] && (row[[j]][["fun"]]=="GroupSize" || colx[["fun"]]=="GroupSize") && row[[j]][["fun"]]!="GroupRate" && colx[["fun"]]!="GroupRate" ){#Alles zieht immer vor GroupSize, Rest geht nicht gemeinsam
          cellcommands[[length(cellcommands)+1]] <- paste0(c(colcommands[["TFstring"]],rowcommands[["TFstring"]]),collapse=" & ")
          names(cellcommands)[length(cellcommands)] <- "TFstring"
          ## Funktion bestimmen die dann ausgefuehrt werden soll
          if(row[[j]][["fun"]]=="GroupSize"){
            cellcommands[[length(cellcommands)+1]] <- colx[["fun"]]
            names(cellcommands)[length(cellcommands)] <- "fun"
          }else if(colx[["fun"]]=="GroupSize"){
            cellcommands[[length(cellcommands)+1]] <- row[[j]][["fun"]]
            names(cellcommands)[length(cellcommands)] <- "fun"
          }
        }else if(colx[["fun"]]=="GroupSize" && row[[j]][["fun"]]=="GroupSize"){###!!!insgesamt ACHTUNG bei ""-Zuweisungen statt NULL, noch was ueberlegen
          cellcommands[[length(cellcommands)+1]] <- paste0(c(colcommands[["TFstring"]],rowcommands[["TFstring"]]),collapse=" & ")
          names(cellcommands)[length(cellcommands)] <- "TFstring"
          cellcommands[[length(cellcommands)+1]] <- "GroupSize"
          names(cellcommands)[length(cellcommands)] <- "fun"
          # if(any(names(rowcommands)%in%"each")){
          #   cellcommands[[length(cellcommands)+1]] <- rowcommands[["each"]]
          #   names(cellcommands)[length(cellcommands)] <- "each"
          # }
        }else if(colx[["fun"]]=="GroupRate" && row[[j]][["fun"]]=="GroupRate"){ # GroupRatexGroupRate-Platzhalter-Wert -> Rechnen hier irgendwas aus, Ergebnis wird eh nicht angezeigt aber fuer unteren Schritt brauchen wir einen cv im Hintergrund
          cellcommands[[length(cellcommands)+1]] <- paste0(c(colcommands[["TFstring"]],rowcommands[["TFstring"]]),collapse=" & ")
          names(cellcommands)[length(cellcommands)] <- "TFstring"
          cellcommands[[length(cellcommands)+1]] <- "GroupSize"
          names(cellcommands)[length(cellcommands)] <- "fun"
        }
        
        # dann fehlen noch Mean und Total
        # 2xvar geht nicht weil z.B. MeanxMean nicht geht
        # ist also in so einem Fall wurscht und rowPriority auch egal weil Zelle sowieso nicht ausgegeben wird
        # MeanxGroupRate und so wird auch so abgefangen
        if(any(names(rowcommands)%in%"var")){
          cellcommands[[length(cellcommands)+1]] <- rowcommands[["var"]]##var=NULL moeglich?abfangen?
          names(cellcommands)[length(cellcommands)] <- "var"
          cellcommands[[length(cellcommands)+1]] <- row[[j]][["fun"]]
          names(cellcommands)[length(cellcommands)] <- "fun"
        }else if(any(names(colcommands)%in%"var")){
          cellcommands[[length(cellcommands)+1]] <- colcommands[["var"]]
          names(cellcommands)[length(cellcommands)] <- "var"
          cellcommands[[length(cellcommands)+1]] <- colx[["fun"]]
          names(cellcommands)[length(cellcommands)] <- "fun"
        }
        cellcommands <- lapply(cellcommands,function(x)if(x[[1]]=="")x[[1]] <- "NULLeinfuellen" else x[[1]] <-x[[1]])
        
        args <- cellcommands[!names(cellcommands)=="fun"]
        for(a in 1:length(cellcommands[!names(cellcommands)=="fun"])){
          args[a] <- paste0(names(cellcommands[!names(cellcommands)=="fun"])[a], "=\"", cellcommands[!names(cellcommands)=="fun"][[a]], "\"")
        }
        args <-  paste0(args, collapse=",")
        args <- gsub("\"NULLeinfuellen\"","NULL",args,fixed=TRUE)
        
        cmd <- paste0(cellcommands[["fun"]],"(dat,",args,")")
        
        cmd <- paste0("out[[j]] <- ",cmd)
        
        #rowname bestimmen:
        if(row[[j]][["fun"]]=="GroupSize"){
          if(!is.null(rowcommands[["TFstring"]])){
            cmd <- paste0(cmd,";names(out)[j]<-","\"",as.name(rowcommands[["TFstring"]]),"\"")
          }else{
            cmd <- paste0(cmd,";names(out)[j]<-\"\"")
          }        
        }else if(row[[j]][["fun"]]=="GroupRate"){
          cmd <- paste0(cmd,";names(out)[j]<-","\"","RATE","\"")
        }else if(row[[j]][["fun"]]=="Mean"){
          cmd <- paste0(cmd,";names(out)[j]<-","\"","MEAN","\"")
        }else if(row[[j]][["fun"]]=="Total"){
          cmd <- paste0(cmd,";names(out)[j]<-","\"","TOTAL","\"")
        }
        
        
        ########################################################################
        
        if(returnCommands){
          cmd <- gsub("out[[j]] <- ",paste0("'row ",j,", col ", whichcol, "': "),cmd,fixed=TRUE)
          cmd_list[[j]] <- cmd
        }else{
          eval(parse(text=cmd))
        }
        
        colcommands<- origcolcommands
        colx <- colx.orig
        row <- row.orig
        rm(cmd,args);gc()
        
      }#ende for-Schleife
      if(returnCommands){
        out <- cmd_list
      }
      if(length(novaluelist)>0){
        out[[length(out)+1]] <- novaluelist
      }
      return(out)
    })
    
    save.out <- out
    
    
    # novaluelist enthaelt die Zeilen und Spalten fuer Zellen die (derzeit) nicht richtig berechnet werden koennen,
    # also z.B. Spalte ist GroupRate und Zeile ist Mean
    if(!all(sapply(out,function(z)length(z))==length(col))){
      novaluelist <- lapply(out,function(x)x[(length(row)+1):max(sapply(out,function(z)length(z)))])
      out <- lapply(out,function(x)x[1:length(row)])
    }  
    
    
    if(!returnCommands){
      
      # if(emptyRows){
      #   
      #   out1 <- lapply(out,function(x)lapply(x,function(y){
      #     if(estimator%in%names(y)){
      #       return(c(NA,y[[estimator]]))
      #     }else{
      #       c(NA,sapply(y,function(z)z[[estimator]]))
      #     }
      #   }))
      #   outErrorval <- lapply(out,function(x)lapply(x,function(y){
      #     if(estimator%in%names(y)){
      #       return(c(NA,y[[error]]))
      #     }else{
      #       c(NA,sapply(y,function(z)z[[error]]))
      #     }
      #   }))
      # }else{
      out1 <- lapply(out,function(x)lapply(x,function(y){
        if(estimator%in%names(y)){
          return(c(y[[estimator]]))
        }else{
          c(sapply(y,function(z)z[[estimator]]))
        }
      }))
      
      
      if(!grepl("cil",error,fixed=TRUE)){
        # 1. Fall error=cv
      outErrorval <- lapply(out,function(x)lapply(x,function(y){
        if(estimator%in%names(y)){
          return(c(y[[error]]))
        }else{
          c(sapply(y,function(z)z[[error]]))
        }
      }))
      }else{
        # 2. Fall error=cil und ciu
        error2 <- gsub("cil","ciu",error,fixed=TRUE)
        outErrorval <- lapply(out,function(x)lapply(x,function(y){
          if(estimator%in%names(y)){
            if(c(y[[estimator]])>=c(y[[error]]) && c(y[[estimator]])<=c(y[[error2]])){
              return(0)
            }else{
              return(2)
            }
          }else{
            c(sapply(y,function(z){
              if(z[[estimator]]>=z[[error]] && z[[estimator]]<=z[[error2]]){
                return(0)
              }else{
                return(2)
              }
            }))
          }
        }))
      }
      
      # }
      
      
      ### Hier setzen wir die Listenelemente auf TRUE, die in der Tabelle nicht angezeigt werden sollen weil 
      ### nicht das berechnet wurde (bzw. werden kann) was in Zeile und Spalte vorgegeben wurde
      ### Also Bsp. Mean in Zeile, GroupRate in Spalte -> Raten von Mittelwerten und sowas sind derzeit nicht vorgesehen
      if(exists("novaluelist")){
        out4 <- out1
        out4 <- lapply(out4,function(x)lapply(x,function(y)y=="abrakadabrasimsalabim"))
        for(k in 1:length(novaluelist)){
          if(is.list(novaluelist[[k]][[1]])){
            for(le in 1:length(novaluelist[[k]][[1]])){
              novaluerow <- as.numeric(novaluelist[[k]][[1]][[le]][names(novaluelist[[k]][[1]][[le]])=="row"])
              out4[[k]][[novaluerow]] <- lapply(out4[[k]][[novaluerow]],function(z)!isTRUE(z))
            }
          }
        }
        out4 <- do.call("cbind",lapply(out4,function(x)do.call("c",x)))  
        
      }
      
      
      if(any(grepl("scaleF",row))){
        
        leout1 <- length(out1)
        out1[[(leout1+1)]] <- list()
        #rowScale <- as.list(rep(NA,length(row)))
        for(k in 1:length(row)){
          if(any(grepl("scaleF",names(row[[k]])))){  
            #rowScale[[k]] <- row[[k]][["scaleF"]]
            out1[[(leout1+1)]][[k]] <- rep(row[[k]][["scaleF"]],length(out1[[leout1]][[k]]))
          }else{
            out1[[(leout1+1)]][[k]] <- rep(NA,length(out1[[leout1]][[k]]))
          }
        }
        out1.2 <- do.call("cbind",lapply(out1,function(x)do.call("c",x)))
        rowScale <- out1.2[,ncol(out1.2)]
        rm(out1.2);gc()
        out1[[length(out1)]] <- NULL
        out1 <- do.call("cbind",lapply(out1,function(x)do.call("c",x)))
        
      }else{
        out1 <- do.call("cbind",lapply(out1,function(x)do.call("c",x)))
      }
      
      
      out2 <- do.call("cbind",lapply(outErrorval,function(x)do.call("c",x)))>lim1
      out3 <- do.call("cbind",lapply(outErrorval,function(x)do.call("c",x)))>lim2
      
      
      out2[is.na(out2)] <- FALSE
      out3[is.na(out3)] <- FALSE
      
      
      
      for(j in 1:ncol(out1)){
        if(!any(grepl("scaleF",row))){
          if("scaleF"%in%names(col[[j]])){
            out1[,j] <- eval(parse(text=paste0("out1[,",j,"]",col[[j]][["scaleF"]]))) 
          }
          if("digits"%in%names(col[[j]])){
            out1[,j] <- round(out1[,j],digits=col[[j]][["digits"]]) 
          }
        }else{
          if("scaleF"%in%names(col[[j]])){
            out1[is.na(rowScale),j] <- eval(parse(text=paste0("out1[is.na(rowScale),",j,"]",col[[j]][["scaleF"]]))) 
            rowind <- which(!is.na(rowScale))
            for(k in rowind){
              out1[k,j] <- eval(parse(text=paste0("out1[k,",j,"]",rowScale[k]))) 
            }
          }
          if("digits"%in%names(col[[j]])){
            out1[,j] <- round(out1[,j],digits=col[[j]][["digits"]]) ## Fuer digits machen wir das ganze Tamtam derzeit nicht
          }
          
          
        }
      }
      
      
      save.out1 <- out1
      if(lim1<lim2){
        if(any(out2)){
          if(is.null(markValue1))
            out1[out2] <- paste0(markLeft1,out1[out2],markRight1)
          else
            out1[out2] <- paste0(markLeft1,markValue1,markRight1) 
        }
        if(any(out3)){
          if(is.null(markValue2))
            out1[out3] <- paste0(markLeft2,save.out1[out3],markRight2)
          else
            out1[out3] <- paste0(markLeft2,markValue2,markRight2) 
        }
      }else{
        if(lim1>lim2)
          warning("\n Achtung: lim1<lim2 ist nicht erfuellt! Falls Absicht, diese Warnung bitte ignorieren.\n")
        
        if(any(out3)){
          if(is.null(markValue2))
            out1[out3] <- paste0(markLeft2,out1[out3],markRight2)
          else
            out1[out3] <- paste0(markLeft2,markValue2,markRight2)
        }
        if(any(out2)){
          if(is.null(markValue1))
            out1[out2] <- paste0(markLeft1,save.out1[out2],markRight1)
          else
            out1[out2] <- paste0(markLeft1,markValue1,markRight1)
        }
      }
      
      if(exists("out4")){
        out4 <- matrix(as.logical(out4),nrow=nrow(out2),ncol=ncol(out2))##?? eh richtige Reihenfolge von Vektor zu Matrix?
        
        out1[out4] <- NA
        
      }
      
      if(!is.null(block)){
        if(!is.null(block[[i]])){
          #         if(!emptyRows){
          #           blocktrenner <- rep(NA,ncol(out1))
          #           out1 <- rbind(blocktrenner,out1)
          #         }
          # if(emptyRows){
          #   rownames(out1)[1]<- paste0("BLOCK_",as.character(as.name(block[[i]])))
          # }
          
        }
        outlist[[length(outlist)+1]] <- out1
        dat <- copy(save.dat) 
      }
    }else{#Ende von !returnCommands   
      if(!is.null(block)){
        outcmd_list[[length(outcmd_list)+1]] <- out
      }
    }
  } #Ende von Block schleife
  if(!is.null(block) && !(returnCommands))
    out1 <- do.call(rbind,outlist)
  else if(!is.null(block) && returnCommands)
    out1 <- outcmd_list
  else if(returnCommands)
    out1 <- out
  #   if(!is.numeric(out1)) #Output herrichten fuer dec="," in csv-Files
  #     out1 <- gsub(".",",",out1,fixed=TRUE)
  
  
  out1
}
