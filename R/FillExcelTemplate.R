Tabelle_bearbeiten <- function(table,startingPoints,nrEmptyRows){
  
  ## Leerzeilen hinzufuegen:
  for(i in 1:length(startingPoints)){
    #cat("i:",i,"\n") 
    
    if(is.vector(table)){
      table <- as.matrix(table,ncol = 1)
    }
    leerzeile <- rep(NA,ncol(table))
    
    if(nrEmptyRows[i]>1){
      leerzeile <-  matrix(rep(NA,ncol(table)*nrEmptyRows[i]),nrow=nrEmptyRows[i]) 
    }  
    if(i==1){
      startrow <- 0
    }else{
      startrow=1
    }
    
    if(ncol(table)==1){
      table <- rbind(as.matrix(table[startrow:(startingPoints[i]-nrEmptyRows[i]-1),]),
                     leerzeile, 
                     as.matrix(table[(startingPoints[i]-nrEmptyRows[i]):nrow(table),]))  
    }else{
      table <- rbind(table[startrow:(startingPoints[i]-nrEmptyRows[i]-1),],
                     leerzeile, 
                     table[(startingPoints[i]-nrEmptyRows[i]):nrow(table),])  
    }
    
  }
  rownames(table)[rownames(table)=="leerzeile"] <- ""
  return(table)
}
seqle <- function(x,incr=1) { 
  if(!is.numeric(x)) x <- as.numeric(x) 
  n <- length(x)  
  y <- x[-1L] != x[-n] + incr 
  i <- c(which(y|is.na(y)),n) 
  list(lengths = diff(c(0L,i)),
       values = x[head(c(0L,i)+1L,-1L)]) 
} 

#table <- customCol

###?? setMissingValue(wb, value = "missing")

#' Funktion befuellt ein Excel Template.
#' 
#' Funktion liest ein Excel-File ein, uebernimmt die Formatvorlage eines ausgewaehlten 
#' Template-Excel-Sheets, befuellt die Kopie dieses Template-Excel-Sheets mit den Ergebnissen aus \link{MakeTable} und liest 
#' das Ergebnis samt urspruenglichem Template-Excel-Sheet (default) und dem neu befuellten Excel-Sheet wieder als
#' Excel-File aus. 
#' 
#' Ein Template-Excel-Sheet, das als Vorlage fuer das zu befuellende Excel-Sheet dient 
#' und i.d.R. leer ist bis auf einige Formate, Header und Rownames, wird durch ein Prefix im Sheet-Namen 
#' gekennzeichnet (\code{prefixTSN}). Der Sheet-Name ist bis auf das Prefix identisch
#' zum Sheet-Namen des zu befuellenden neuen Excel-Sheets. Sollte entweder Template oder 
#' zu befuellendes neues Excel-Sheet noch nicht existieren, wird es automatisch 
#' angelegt. Die Template-Sheets koennen bei Bedarf wieder einzeln (\code{removeTemplateSheet}) 
#' oder alle auf einmal (removeAllTemplates) geloescht werden. Sollten sowohl das Template 
#' als auch das neue Sheet im File schon existieren ist es irrelevant ob bei \code{sheet} 
#' das Template oder das neue Sheet angegeben wird. 

#' 
#' Derzeit funktioniert diese Funktion nur fuer die Default-Werte von
#' \code{markLeft1}, \code{markRight1}, \code{markValue1}, \code{markLeft2},
#' \code{markRight2} und \code{markValue2} aus \code{MakeTable()} und
#' \code{MakeAKETimeInstantsTable()}.
#' 
#' @param tab1 eine mit \code{MakeTable()} bzw.
#' \code{MakeAKETimeInstantsTable()} erzeugte Tabelle. Falls bei
#' \code{MakeTable()} limits angegeben und einzelne Zellen mit Klammern oder
#' Aehnlichem belegt wurden muss auch tab2 angegeben werden damit man die Werte
#' in den Zellen bekommt.
#' @param tab2 NULL oder eine mit \code{MakeTable()} bzw.
#' \code{MakeAKETimeInstantsTable()} erzeugte Tabelle bei der limits NICHT
#' angegeben wurden. Diese Tabelle muss immer uebergeben werden wenn fuer die
#' Erstellung von tab1 limits beruecksichtigt wurden.
#' @param startingPoints numerischer Vektor: die Startzeilen der befuellten
#' Zeilen nach Leerzeilen im Original-Excel-File.
#' @param nrEmptyRows numerischer Vektor: Anzahl an Leerzeilen die vor
#' \code{startingPoints} kommen sollen; eigentlich immer 1 ausser vor grossen
#' Bloecken.
#' @param inheritTemplateColNr numerischer Vektor: Spaltennummer/n der Tabellenspalten die vom Original-Excel-File uebernommen werden sollen.
#' Default ist die erste Spalte, also \code{inheritTemplateColNr=1}.
#' @param customColNr numerischer Wert: Spaltennummer der Tabellenspalte die ueber \code{customCol} individuell definiert werden soll (derzeit nur EINE moeglich). 
#' @param customCol character Vektor: enthaelt die Eintraege der durch \code{customColNr} definierten Tabellenspalte - falls diese 
#' nicht aus dem Original-Excel-File uebernommen werden sollen und auch nicht durch \code{MakeTable()} generiert werden. 
#' Derzeit (kein Bedarf) wird hier ein Character Vektor OHNE Missings uebergeben, die ueber \code{startingPoints} 
#' und \code{nrEmptyRows} definierten Leerzeilen werden also einfach uebernommen.
#' @param footnote character: Fussnote (derzeit fix 1-te Zelle der 2-ten Zeile nach Tabellenende), falls sie nicht aus dem Original-Excel-File ubernommen werden soll.
#' Formatierung der Zelle der die Fussnote zugewiesen wird bleibt allerdings wie im Original-Excel-File.
#' @param f_in File Name inklusive Pfad und File-Endungen des eingelesenen
#' Original-Excel-Files.
#' @param sheet Index oder Name des Excel-Sheets oder des zugehoerigen Template-Excel-Sheets.
#' @param prefixTSN Character: das Prefix des Namens des Template Sheets (siehe Details). 
#' Default ist "_". 
#' @param removeTemplateSheet TRUE/FALSE ob das Template-Excel-Sheet (mit dem Prefix \code{prefixTSN}) zum aktuell 
#' bearbeiteten Excel-Sheet geloescht werden soll, also ob es nicht im ausgelesenen File enthalten sein soll.
#' @param removeAllTemplates TRUE/FALSE wie bei \code{removeTemplateSheet}, nur dass hier abgefragt wird, 
#' ob ALLE Template-Excel-Sheets, also alle Sheets mit dem Prefix \code{prefixTSN},
#' geloescht werden sollen, also ob das ausgelesene File keine Templates mehr enthalten soll.
#' @param interactive Logical ob das Loeschen von Template-Sheets (\code{removeTemplateSheet},\code{removeAllTemplates}) 
#' erst manuell durch den Nutzer bestaetigt werden soll.
#' @param showFinalTab Logical: Falls TRUE, wird in R die Tabelle samt Leerzeilen ausgegeben wie 
#' sie auch im ausgelesenen Excel-File landen wuerde. Ist dieser Parameter gesetzt, wird also kein Excel-File erstellt.
#' @param showSplitTab Logical: Falls TRUE, wird in R die durch \code{startingPoints} aufgesplittete Tabelle ausgegeben. 
#' Ist dieser Parameter gesetzt, wird also kein Excel-File erstellt.
#' @return Output ist ein Excel-File.
#' @seealso
#' \code{\link{MakeTable},\link{MakeAKETimeInstantsTable},\link{ImportData},\link{IndivImportData}}
#' @export
#' @examples
#' \dontrun{
#' ###
#' Kommt wahrscheinlich ins mitgelieferte Bsp-File - samt Excel-Rohling.
#' ###
#' }
#' 
FillExcelTemplate <- function(tab1,tab2=NULL,startingPoints,nrEmptyRows,
                              inheritTemplateColNr=1,customColNr=NULL,customCol=NULL,
                              footnote=NULL,f_in,sheet=1,prefixTSN="_",
                              removeTemplateSheet=FALSE,removeAllTemplates=FALSE,interactive=TRUE,
                              showFinalTab=FALSE,showSplitTab=FALSE){
  if(!removeAllTemplates){
    
    ## Fehler abfangen
    if(!file.exists(f_in)){
      stop("\n\nFile '",f_in,"' existiert nicht und kann daher nicht eingelesen werden!\n")
    }
    if(!is.null(customCol) && is.null(customColNr)){
      stop("\n\nZu customCol muss eine customColNr spezifiziert werden. Siehe Help-File!\n")
    }
    if(!is.null(inheritTemplateColNr) && !is.null(customCol)){
      if(customColNr %in% inheritTemplateColNr){
        stop("\n\ncustomColNr darf nicht gleich inheritTemplateColNr sein!\n")
      }
    }
    if(!is.null(customCol)){
      if(length(customCol)!=nrow(tab1)){
        stop("\n\nDer Vektor customCol muss gleich viele Elemente haben wie tab1 Zeilen hat!\n")
      }
      if(length(customColNr)>1){
        stop("\n\nDerzeit kann nur EINE customColNr spezifiziert werden!\n")
      }
      if(identical(customColNr,0)){
        customCol <- NULL
        customColNr <- NULL
      }
    }
    if(!is.null(inheritTemplateColNr)){
      if(any(inheritTemplateColNr>(ncol(tab1)+length(inheritTemplateColNr)+1))){
        #bzw +length(customColNr) - falls wir das mal aendern
        warning("\n\n\nACHTUNG: inheritTemplateColNr ",
                paste0(inheritTemplateColNr[which(inheritTemplateColNr>(ncol(tab1)+length(inheritTemplateColNr)+1))],collapse=", "),
                " ist zu gross (ausserhalb der Tabelle) und wird aus inheritTemplateColNr entfernt!!!!\n\n")
        
        inheritTemplateColNr <- inheritTemplateColNr[-which(inheritTemplateColNr>(ncol(tab1)+length(inheritTemplateColNr)+1))]
        
        if(length(inheritTemplateColNr)==0){
          inheritTemplateColNr <- NULL
        }
      }   
      if(0%in%inheritTemplateColNr){
        if(identical(customColNr,0)){
          inheritTemplateColNr <- NULL
        }else{
          stop("inheritTemplateColNr enthaelt den Wert 0, das macht keinen Sinn :-( !\n")
        }
      }
    }
    
    tab1ColNr <- seq(1:(ncol(tab1)+length(inheritTemplateColNr)+length(customColNr)))
    if(!is.null(inheritTemplateColNr) || !is.null(customColNr)){
      tab1ColNr <- tab1ColNr[-c(inheritTemplateColNr,customColNr)]  
    }
    
    
    ## Leerzeilen zu tab1 und tab2 hinzufuegen
    erg <- Tabelle_bearbeiten(tab1,startingPoints=startingPoints,nrEmptyRows=nrEmptyRows)
    if(is.null(tab2)){
      erg2 <- copy(erg)
    }else{
      erg2 <- Tabelle_bearbeiten(tab2,startingPoints=startingPoints,nrEmptyRows=nrEmptyRows)
    }
    
    if(showFinalTab){
      return(erg)
    }
    
    ######################################################################
    ##    3. Excel-File inklusive dort vorgegebener Formate einlesen   ##
    ######################################################################
    # Excel-File Einlesen
    wb <- loadWorkbook(f_in, create=TRUE)
    cat("\n",f_in," wird eingelesen.\n")
    setStyleAction(wb,XLC$"STYLE_ACTION.NONE") #dadurch werden die vorgegebenen Formate beibehalten
    sheets <- getSheets(wb)
    
    # Helper Function: wollen sheet als number und nicht als character. 
    # Ausserdem wollen wir auf Nummer Sicher gehen, dass das von uns uebergebene sheet ueberhaupt existiert!
    sheet_as_number <- function(sheets=sheets, sheet=sheet, prefixTSN=prefixTSN){
      if(is.character(sheet)){
        sheet.orig <- sheet
        sheet <- which(sheets==sheet)
        # falls nur _-sheet existiert, aber zu befuellender sheet-Name angegeben wurde und umgekehrt
        if(length(sheet)==0 && !paste0(prefixTSN,sheet.orig)%in%sheets && !substr(sheet.orig,2,nchar(sheet.orig))%in%sheets){
          stop("Excel-Sheet '",sheet.orig,"' kann nicht gefunden werden!")  
        }else if(length(sheet)==0 && paste0(prefixTSN,sheet.orig)%in%sheets){
          sheet.orig <- sheet <- paste0(prefixTSN,sheet.orig)
          sheet <- which(sheets==sheet)
        }else if(length(sheet)==0 && substr(sheet.orig,2,nchar(sheet.orig))%in%sheets){
          sheet.orig <- sheet <- substr(sheet.orig,2,nchar(sheet.orig))
          sheet <- which(sheets==sheet)
        }
      }else{
        sheet.orig <- sheets[sheet]
      }
      list(sheet=sheet, sheet.orig=sheet.orig)
    }
    
    sheet <- sheet_as_number(sheets=sheets, sheet=sheet, prefixTSN=prefixTSN)$sheet
    sheet.orig <- sheet_as_number(sheets=sheets, sheet=sheet, prefixTSN=prefixTSN)$sheet.orig
    
    # Falls zu befuellendes sheet schon existiert, sollten wir dieses loeschen und als kopie von _-sheet neu erstellen
    # Ansonsten wird ja das Format (also evt. Klammern usw. vom frueher schon mal befuellten befuellten Sheet genommen)
    if((existsSheet(wb,substr(sheets[sheet],2,nchar(sheets[sheet]))) && existsSheet(wb,sheets[sheet]))){
      removeSheet(wb,substr(sheets[sheet],2,nchar(sheets[sheet])))
      sheets <- getSheets(wb)
      sheet <- sheet.orig
    }else if(existsSheet(wb,paste0(prefixTSN,sheets[sheet])) && existsSheet(wb,sheets[sheet])){
      removeSheet(wb,sheets[sheet])
      sheets <- getSheets(wb)
      sheet <- sheet.orig
    }
    # Wieder Zahl statt character fuer sheet und Kontrolle von uebergebenem sheet-Name
    sheet <- sheet_as_number(sheets=sheets, sheet=sheet, prefixTSN=prefixTSN)$sheet
    sheet.orig <- sheet_as_number(sheets=sheets, sheet=sheet, prefixTSN=prefixTSN)$sheet.orig
    
    # leeres sheet generieren, falls es noch keines gibt
    if(!existsSheet(wb,paste0(prefixTSN,sheets[sheet])) && substr(sheets[sheet],1,1)!=prefixTSN){
      cloneSheet(wb,sheets[sheet],name=paste0(prefixTSN,sheets[sheet]))
      sheets <- getSheets(wb)
    }  
    # zu befuellendes sheet generieren falls es nur das leere gibt
    if(substr(sheets[sheet],1,1)==prefixTSN && !existsSheet(wb,substr(sheets[sheet],2,nchar(sheets[sheet])))){
      newsheetname <- substr(sheets[sheet],2,nchar(sheets[sheet]))
      cloneSheet(wb,sheets[sheet],name=newsheetname)
      sheets <- getSheets(wb)
      sheet <- which(sheets==newsheetname)
    }  
    # leeres _-sheet immer vor zu befuellendes sheet stellen (noch mal kontrollieren diesen Teil hier)
    if(getSheetPos(wb,sheets[sheet]) != (getSheetPos(wb,paste0(prefixTSN,sheets[sheet]))+1)){
      if(getSheetPos(wb,sheets[sheet]) < getSheetPos(wb,paste0(prefixTSN,sheets[sheet]))){
        if(getSheetPos(wb,sheets[sheet])-1!=0)
          setSheetPos(wb,paste0(prefixTSN,sheets[sheet]),getSheetPos(wb,sheets[sheet]))
        else
          setSheetPos(wb,paste0(prefixTSN,sheets[sheet]),1)
      }else{
        newPosition <- getSheetPos(wb,paste0(prefixTSN,sheets[sheet]))+1
        setSheetPos(wb,sheets[sheet],newPosition) 
        sheet <- newPosition
      }
      sheets <- getSheets(wb)
    }
    # aktives Sheet soll das neu zu befuellende sein
    if(substr(sheets[which(sheets==sheet.orig)],1,1)==prefixTSN){
      if(substr(sheets[sheet],1,1)==prefixTSN){
        newsheetname <- substr(sheets[sheet],2,nchar(sheets[sheet]))
        sheet <- which(sheets==newsheetname)
      }
      setActiveSheet(wb,sheets[sheet])
    }else{
      sheet <- which(sheets==sheet.orig) 
      setActiveSheet(wb,sheets[sheet])
    }
    
    cat("\n",sheets[sheet], " wird bearbeitet.\n")
    
    
    # save.erg <- copy(erg)
    # save.erg2 <- copy(erg2)
    
    
    if((!is.null(inheritTemplateColNr) && !any(inheritTemplateColNr==0)) || !is.null(customCol)){
      prepare_out_dt <- function(dt,inheritTemplateColNr, customCol, tab1ColNr){
        
        ncol_newdt <- length(c(inheritTemplateColNr, customColNr, tab1ColNr))
        newdt <- data.table(matrix(nrow=nrow(dt),ncol=ncol_newdt))
        #newdt[,(colnames(newdt)):=lapply(.SD, as.character),.SDcols=colnames(newdt)]
        newdt[,tab1ColNr] <- data.table(dt)
        if(!is.null(customCol) && identical(dt,erg)){
          newdt[,customColNr]  <- Tabelle_bearbeiten(customCol,startingPoints=startingPoints,nrEmptyRows=nrEmptyRows)
        }
        colnames(newdt) <- LETTERS[1:ncol(newdt)]
        return(newdt)
        
      }
      erg <- prepare_out_dt(erg,inheritTemplateColNr,customCol,tab1ColNr)   
      erg2 <- prepare_out_dt(erg2,inheritTemplateColNr,customCol,tab1ColNr)  
    }else{
      colnames(erg) <- LETTERS[1:ncol(erg)]
      colnames(erg2) <- LETTERS[1:ncol(erg2)]
      erg <- as.data.table(erg)
      erg2 <- as.data.table(erg2)
    }
    
    # erg <- copy(save.erg)
    # erg2 <- copy(save.erg2)
    
    
    # Befuellte Zeilen aus erg heraussuchen
    zeilen_mit_inhalt <- as.numeric(which(apply(erg,1,function(x)any(!is.na(x)))))
    #zeilen_mit_inhalt <- as.numeric(which(apply(erg,1,function(x)all(!is.na(x)))))
    outlist <- list()
    i_orig <- i <- 1
    
    while(i<=length(zeilen_mit_inhalt)){
      while((zeilen_mit_inhalt[i]+1)%in%zeilen_mit_inhalt){
        i <- i+1
      }
      outlist[[length(outlist)+1]] <- erg[zeilen_mit_inhalt[i_orig]:zeilen_mit_inhalt[i],]  
      i_orig <- i+1
      i <- i+1
    }
    
    if(showSplitTab){
      return(outlist)
    }
    outlist.orig <- outlist
    
    
    # da Klammern und x-e in erg vorkommen, sind die Zellenwerte nicht numerisch. Das wollen wir wieder aendern.
    # durch as.numeric kommt es bei Zellen mit Characterwerten zu Missings -> nicht beunruhigend, die befuellen wir spaeter
    # Output hier ist uebrigens eine Liste mit data.table-Elementen
    if(!is.null(customColNr)){
      whichcols <- colnames(outlist[[1]])[-customColNr]
    }else{
      whichcols <- colnames(outlist[[1]])
    }
    
    outlist <- lapply(outlist, function(x){
      if(nrow(x)>1){
        x[ ,(whichcols):=lapply(.SD,function(y) suppressWarnings(as.numeric(y))), .SDcols=whichcols] 
      }else if(nrow(x)==1){
        x[ ,(whichcols):=lapply(.SD,function(y) suppressWarnings(as.numeric(y))), .SDcols=whichcols] 
      }
    })
    
    
    if(!is.null(footnote)){
      writeWorksheet (wb, footnote, sheet=sheets[sheet], startRow=nrow(erg)+2, startCol=1 ,header=FALSE )
    }
    
    
    # wir suchen die ausgeklammerten Zellenwerte usw. 
    ausgeklammertes <- apply(erg,2,function(x)grep("(",x,fixed=TRUE)) # alle, d.h. (Wert) und (x)
    ausgeklammertes_x <- apply(erg,2,function(x)grep("(x)",x,fixed=TRUE)) # (x)
    ausgeklammertes_stern <- apply(erg,2,function(x)grep("*",x,fixed=TRUE)) # (x)
    zelle_leer <- apply(erg,2,function(x){which(is.na(x))[which(which(is.na(x))%in%zeilen_mit_inhalt)]}) 
    wert_null <- apply(erg2,2,function(x)which(abs(x) < .Machine$double.eps)) # Zellenwert 0
    notanumber <- apply(erg,2,function(x)grep("NaN",x,fixed=TRUE)) # NaN
    #c(ausgeklammertes,ausgeklammertes_x,ausgeklammertes_stern,zelle_leer,wert_null,wert_null)
    
    removeFlag <- function(x,colNr){
      for(i in 1:length(colNr)){
        if(length(x)>0){
          x[[colNr[i]]] <- grep("kreizbirnbaumhollastaudn","bla")
        }
      }
      return(x)
    }
    
    if(!is.null(customCol)){
      # Spezielle Zellenmarkierungen sollen fuer customCol NICHT gelten. Entfernen also alle eventuell unabsichtlich auftretenden Markierungen.
      ausgeklammertes <- removeFlag(ausgeklammertes,colNr=customColNr)
      ausgeklammertes_x <- removeFlag(ausgeklammertes_x,colNr=customColNr)
      ausgeklammertes_stern <- removeFlag(ausgeklammertes_stern,colNr=customColNr)
      zelle_leer <- removeFlag(zelle_leer,colNr=customColNr)
      wert_null <- removeFlag(wert_null,colNr=customColNr)
      notanumber <- removeFlag(notanumber,colNr=customColNr)
    }
    if(!is.null(inheritTemplateColNr) && !any(inheritTemplateColNr==0)){
      # Spezielle Zellenmarkierungen sollen fuer inheritTemplateColNr NICHT gelten. Entfernen also alle eventuell unabsichtlich auftretenden Markierungen.
      ausgeklammertes <- removeFlag(ausgeklammertes,colNr=inheritTemplateColNr)
      ausgeklammertes_x <- removeFlag(ausgeklammertes_x,colNr=inheritTemplateColNr)
      ausgeklammertes_stern <- removeFlag(ausgeklammertes_stern,colNr=inheritTemplateColNr)
      zelle_leer <- removeFlag(zelle_leer,colNr=inheritTemplateColNr)
      wert_null <- removeFlag(wert_null,colNr=inheritTemplateColNr)
      notanumber <- removeFlag(notanumber,colNr=inheritTemplateColNr)
    }
    
    
    writeColNr <- sort(c(tab1ColNr,customColNr))
    for(i in 1:length(startingPoints)){
      # sel_seq <- seqle(writeColNr)
      # values <- sel_seq$values[which(sel_seq$lengths>1)]
      # lengths <- sel_seq$lengths[which(sel_seq$lengths>1)]
      for(j in writeColNr){
        writeWorksheet (wb, outlist[[i]][,j,with=FALSE], sheet=sheets[sheet], startRow=startingPoints[i], startCol=j ,header=FALSE )
      }
    }
    
    # Koennten ein Format setzen fuer ausgeklammerte Werte:
    klammern <- createCellStyle(wb)
    klammern_x <- createCellStyle(wb)
    stern <- createCellStyle(wb)
    kein_eintrag <- createCellStyle(wb)
    null_mit_klammern <- createCellStyle(wb)
    
    #setDataFormat(klammern, format = "(#.##0,0);(-#.##0,0);@")# deutsches Excel -> macht das daraus:(#,##00);(-#,##00);@
    setDataFormat(klammern, format = "(#,##0.0);(-#,##0.0);@")# englisches Excel
    setDataFormat(klammern_x, format = "(x);(x)")
    setDataFormat(stern, format = "#,##0.0\"*\"")
    setDataFormat(kein_eintrag, format = "0.0") #Zellen ohne Eintrag sollen "." enthalten. 
    setDataFormat(null_mit_klammern, format = "\"[\"0\"]\";\"[\"0\"]\"") #Zellen sollen Format [0] bekommen. 
    
    # Jetzt zur Extrawurst fuer die ausgeklammerten Werte
    ersteZeile <- 1
    #-> Erste Zeile in Excel-Sheet ab der erg als gesamter Block (also inklusive der ersten Zeile mit NAs) eingefuegt wird. 
    # Das ist 1, weil wir auch alle noetigen Leerzeilen schon in erg hinzugefuegt haben.
    
    ### Hier machen wir aus den ausgeklammerten Werten erst mal wieder numerische Werte um formatC() darauf anzuwenden.
    # Dann werden die fehlenden Zellen im workbook Zelle fuer Zelle befuellt
    # Hier auch wieder nicht schrecken, wenn warnings wegen as.numeric ausgegeben werden. 
    # Die Missings die daruch entstehen sind hier auch egal.
    if(length(ausgeklammertes)>0){
      if(is.null(tab2)){
        stop("\ntab2 muss angegeben werden da fuer tab1 ein Limit gesetzt wurde und gewisse Zellen keinen numerischen Wert enthalten!\n")
      }
      for( i in 1:length(ausgeklammertes)){
        if(length(unlist(ausgeklammertes[i]))>0){
          for(j in 1:length(unlist(ausgeklammertes[i]))){
            wert.orig <- wert <- as.character(erg[suppressWarnings(as.numeric(unlist(ausgeklammertes[i])[j])),names(ausgeklammertes)[i],with=F])
            wert <- gsub("(","",wert,fixed=TRUE)
            wert <- gsub(")","",wert,fixed=TRUE)
            if(!is.na(suppressWarnings(as.numeric(wert)))){
              wert <- as.numeric(wert)
              writeWorksheet (wb, wert, sheet=sheets[sheet], startRow=as.numeric(unlist(ausgeklammertes[i])[j])+(ersteZeile-1), startCol=grep(names(ausgeklammertes)[i],LETTERS) ,header=FALSE )
              setCellStyle(wb, sheet=sheets[sheet], row=as.numeric(unlist(ausgeklammertes[i])[j])+(ersteZeile-1), col=grep(names(ausgeklammertes)[i],LETTERS), cellstyle=klammern)
            }
          }
        }
      }
    }
    if(length(ausgeklammertes_stern)>0){
      if(is.null(tab2)){
        stop("\ntab2 muss angegeben werden da fuer tab1 ein Limit gesetzt wurde und gewisse Zellen keinen numerischen Wert enthalten!\n")
      }
      for( i in 1:length(ausgeklammertes_stern)){
        if(length(unlist(ausgeklammertes_stern[i]))>0){
          for(j in 1:length(unlist(ausgeklammertes_stern[i]))){
            wert.orig <- wert <- as.character(erg[suppressWarnings(as.numeric(unlist(ausgeklammertes_stern[i])[j])),names(ausgeklammertes_stern)[i],with=F])
            wert <- gsub("*","",wert,fixed=TRUE)
            if(!is.na(suppressWarnings(as.numeric(wert)))){
              wert <- as.numeric(wert)
              writeWorksheet (wb, wert, sheet=sheets[sheet], startRow=as.numeric(unlist(ausgeklammertes_stern[i])[j])+(ersteZeile-1), startCol=grep(names(ausgeklammertes_stern)[i],LETTERS) ,header=FALSE )
              setCellStyle(wb, sheet=sheets[sheet], row=as.numeric(unlist(ausgeklammertes_stern[i])[j])+(ersteZeile-1), col=grep(names(ausgeklammertes_stern)[i],LETTERS), cellstyle=stern)
            }
          }
        }
      }
    }
    
    ### (x) nur als label darueberlegen
    if(length(ausgeklammertes_x)>0){
      if(is.null(tab2)){
        stop("\ntab2 muss angegeben werden da fuer tab1 ein Limit gesetzt wurde und gewisse Zellen keinen numerischen Wert enthalten!\n")
      }
      
      for( i in 1:length(ausgeklammertes_x)){
        if(length(unlist(ausgeklammertes_x[i]))>0){
          for(j in 1:length(unlist(ausgeklammertes_x[i]))){
            wert <- as.numeric(erg2[suppressWarnings(as.numeric(unlist(ausgeklammertes_x[i])[j])),names(ausgeklammertes_x)[i],with=F])
            writeWorksheet (wb, wert, sheet=sheets[sheet], startRow=as.numeric(unlist(ausgeklammertes_x[i])[j])+(ersteZeile-1), startCol=grep(names(ausgeklammertes_x)[i],LETTERS) ,header=FALSE )
            setCellStyle(wb, sheet=sheets[sheet], row=as.numeric(unlist(ausgeklammertes_x[i])[j])+(ersteZeile-1), col=grep(names(ausgeklammertes_x)[i],LETTERS), cellstyle=klammern_x)
          }
        }
      }
    }
    ## Wert 0 soll in eckige Klammern kommen
    if(length(wert_null)>0){
      for( i in 1:length(wert_null)){
        if(length(unlist(wert_null[i]))>0){
          for(j in 1:length(unlist(wert_null[i]))){
            wert <- 0
            writeWorksheet (wb, wert, sheet=sheets[sheet], startRow=as.numeric(unlist(wert_null[i])[j])+(ersteZeile-1), startCol=grep(names(wert_null)[i],LETTERS) ,header=FALSE )
            setCellStyle(wb, sheet=sheets[sheet], row=as.numeric(unlist(wert_null[i])[j])+(ersteZeile-1), col=grep(names(wert_null)[i],LETTERS), cellstyle=null_mit_klammern)
          }
        }
      }
    }
    ## Statt NaN bzw leerer Zelle soll 0 in eckige Klammern kommen
    if(length(notanumber)>0){
      for( i in 1:length(notanumber)){
        if(length(unlist(notanumber[i]))>0){
          for(j in 1:length(unlist(notanumber[i]))){
            wert <- 0 #Im Hintergrund soll 0 stehen?
            writeWorksheet (wb, wert, sheet=sheets[sheet], startRow=as.numeric(unlist(notanumber[i])[j])+(ersteZeile-1), startCol=grep(names(notanumber)[i],LETTERS) ,header=FALSE )
            setCellStyle(wb, sheet=sheets[sheet], row=as.numeric(unlist(notanumber[i])[j])+(ersteZeile-1), col=grep(names(notanumber)[i],LETTERS), cellstyle=null_mit_klammern)
          }
        }
      }
    }
    
    ### Statt leerer Zelle soll ein Punkt angezeigt werden der aber als Zahl formatiert ist.
    if(length(zelle_leer)>0){
      for( i in 1:length(zelle_leer)){
        if(length(unlist(zelle_leer[i]))>0){
          for(j in 1:length(unlist(zelle_leer[i]))){
            wert <- "."
            writeWorksheet (wb, wert, sheet=sheets[sheet], startRow=as.numeric(unlist(zelle_leer[i])[j])+(ersteZeile-1), startCol=grep(names(zelle_leer)[i],LETTERS) ,header=FALSE )
            setCellStyle(wb, sheet=sheets[sheet], row=as.numeric(unlist(zelle_leer[i])[j])+(ersteZeile-1), col=grep(names(zelle_leer)[i],LETTERS), cellstyle=kein_eintrag)
          }
        }
      }
    }
    
    if(removeTemplateSheet){
      loeschen <- which(sheets==paste0(prefixTSN,sheets[sheet]))
      if(interactive){
        cat("\nSoll das Excel-Sheet ",sheets[loeschen], " wirklich geloescht werden?\n")
        answer <- "a"
        while(!tolower(answer)%in%c("nein","n","ja","j")){    
          answer <- readline(prompt="Bitte ja oder nein eingeben: \n")
          if(tolower(answer)%in%c("nein","n"))
            stop("\nLoeschen des Template-Excel-Sheets ",sheets[loeschen]," wird abgebrochen!\n",call.=FALSE)
          else if(tolower(answer)%in%c("ja","j")){
            answer2 <- "a"
            while(!tolower(answer2)%in%c("nein","n","ja","j")){    
              answer2 <- readline(prompt="Soll vor dem Loeschen des Template-Sheets eine Sicherheitskopie des Files angelegt werden?: \n")
              if(tolower(answer2)%in%c("ja","j")){
                newfile <- unlist(strsplit(basename(f_in),".",fixed=TRUE))[length(unlist(strsplit(basename(f_in),".",fixed=TRUE)))-1]
                fileExtension <- unlist(strsplit(basename(f_in),".",fixed=TRUE))[length(unlist(strsplit(basename(f_in),".",fixed=TRUE)))]
                newfile <- paste0(dirname(f_in),"/",newfile,"_kopie.",fileExtension)
                n <- 1
                while(file.exists(newfile)){
                  newfile2 <- unlist(strsplit(basename(newfile),".",fixed=TRUE))[length(unlist(strsplit(basename(newfile),".",fixed=TRUE)))-1]
                  fileExtension <- unlist(strsplit(basename(newfile),".",fixed=TRUE))[length(unlist(strsplit(basename(newfile),".",fixed=TRUE)))]
                  newfile <- paste0(dirname(newfile),"/",newfile2,"_(",n,").",fileExtension)
                  n <- n+1
                }
                saveWorkbook(wb,file=newfile)
              }
            }
            
          }
        }
      }
      removeSheet(wb,sheet=loeschen)
    }
    
  }else{#ende not finalize file
    cat("\n",f_in," wird eingelesen.\n")
    # Excel-File Einlesen
    wb <- loadWorkbook(f_in, create=TRUE)
    setStyleAction(wb,XLC$"STYLE_ACTION.NONE") #dadurch werden die vorgegebenen Formate beibehalten
    sheets <- getSheets(wb)
    loeschen <- which(substr(sheets,1,1)==prefixTSN)
    if(interactive){
      
      cat("\nSollen die Template-Excel-Sheets ",sheets[loeschen], " wirklich geloescht werden?\n")
      
      answer <- "a"
      while(!tolower(answer)%in%c("nein","n","ja","j")){    
        answer <- readline(prompt="Bitte ja oder nein eingeben: \n")
        if(tolower(answer)%in%c("nein","n"))
          stop("\nLoeschen der Template-Excel-Sheets wird abgebrochen!\n",call.=FALSE)
        else if(tolower(answer)%in%c("ja","j")){
          answer2 <- "a"
          while(!tolower(answer2)%in%c("nein","n","ja","j")){    
            answer2 <- readline(prompt="Soll vor dem Loeschen der Template-Sheets eine Sicherheitskopie des Original-Files angelegt werden?: \n")
            if(tolower(answer2)%in%c("ja","j")){
              newfile <- unlist(strsplit(basename(f_in),".",fixed=TRUE))[length(unlist(strsplit(basename(f_in),".",fixed=TRUE)))-1]
              fileExtension <- unlist(strsplit(basename(f_in),".",fixed=TRUE))[length(unlist(strsplit(basename(f_in),".",fixed=TRUE)))]
              newfile <- paste0(dirname(f_in),"/",newfile,"_kopie.",fileExtension)
              n <- 1
              while(file.exists(newfile)){
                newfile2 <- unlist(strsplit(basename(newfile),".",fixed=TRUE))[length(unlist(strsplit(basename(newfile),".",fixed=TRUE)))-1]
                fileExtension <- unlist(strsplit(basename(newfile),".",fixed=TRUE))[length(unlist(strsplit(basename(newfile),".",fixed=TRUE)))]
                newfile <- paste0(dirname(newfile),"/",newfile2,"_(",n,").",fileExtension)
                n <- n+1
              }
              saveWorkbook(wb,file=newfile)
            }
          }
        }
        
      }
    }
    loeschen <- loeschen-seq(0,length(loeschen)-1,by=1) ##removeSheet loescht nicht alle Spalten auf einmal die man angibt sondern iterativ, d.h. loeschen Spalte 3 bezieht sich nach dem Loeschen von Spalte 1 auf die urspruengliche Spalte 4.
    removeSheet(wb,sheet=loeschen)
    
  }  
  #removeSheet(wb,sheets[1])
  saveWorkbook(wb,file=f_in)
  cat("\n",f_in," wird wieder ausgelesen.\n")
  cat("\n[fertig]\n")
  cat("\n")
  
}
