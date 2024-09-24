# Helper Functions
# Die RND Funktion in SPSS rundet bei 5 immer weg von 0 (also bei positiven Zahlen immer auf, bei negativen Zahlen immer ab)
round.spss = function(x, digits=0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(as.numeric(as.character(z))) # as.numeric(as.character(z)) wegen floating-point numbers
  z = z/10^digits
  z*posneg
}

# vollstaendige Quartale (inkl. Bootstrap-Gewichten)
vorhQuartaleUndPfade <- function(mz_intern) {
  # Pfade
  p1 <- paste0(mz_intern, "/")
  dir_gew <- paste0(p1, "XXXX/XXXXqYY")
  
  ## alle moeglichen Jahre/Quartale herausfiltern
  nn <- tolower(list.files(p1,include.dirs =TRUE,recursive = TRUE))
  nn <- nn[grep("dg7.mz....q..sav$",nn)]
  nn <- nn[which(nchar(nn) == 28)]
  nn <- unlist(lapply(strsplit(nn,"dg7.mz"),function(x)x[2]))
  nn <- unlist(lapply(strsplit(nn,".sav"),function(x)x[1]))
  
  # dircurrb des letztes quartals
  dircurrb <- gsub("XXXX",substr(tail(nn,1),1,4), dir_gew)
  dircurrb <- gsub("YY",substr(tail(nn,1),6,6), dircurrb)
  
  # existieren fuer das letztes Jahr/Quartal die Gewichte?
  # ansonsten rausschmeissen
  fIn <- paste0(dircurrb,"/mz2_",tail(nn,1),"_bootweights.csv.gz")
  if ( !file.exists(fIn) ) {
    nn <- nn[-length(nn)]
  }
  
  out <- lapply(1:length(nn), function(x) {
    jj <- substr(nn[x],1,4)
    qq <- substr(nn[x],6,6)
    dircurr <- gsub("XXXX",jj, dir_gew)
    dircurr <- gsub("YY",qq, dircurr)
    list(
      jahr=substr(nn[x],1,4),
      quartal=substr(nn[x],6,6),
      pfad_dg7=paste0(dircurr,"/dg7.mz",nn[x],".sav"),
      pfad_gew=paste0(dircurr,"/mz2_",nn[x],"_bootweights.csv.gz")
    )
  })
  names(out) <- nn
  return(out)
}

mount_mz_intern <- function() {
  b_mz2 <- getFolder("mz2")
  file.path(b_mz2, "20_MZ","MZ_intern")
}

# Output ist eine Liste mit einem oder zwei Elementen, je nachdem ob comp_diff_lag angegeben wurde oder nicht.



#' Mikrozensus-Files und zugehoerige Bootstrapgewichte einlesen (hausintern).
#' 
#' Funktion liest Mikrozensus-Files (dg7) und zugehoerige Bootstrapgewichte ein
#' (basierend auf STAT-Filemanagement, d.h. diese Funktion funktioniert nur STAT-intern).
#' 
#' 
#' @param year Numerischer Wert (Jahr).
#' @param quarter Numerischer Wert (Quartal) oder NULL. Falls NULL, wird das
#' ganze Jahr eingelesen.
#' @param comp_diff_lag Numerischer Wert oder NULL. Falls NULL, wird keine
#' Fehlerrechnung fuer Veraenderungen zwischen zwei Zeitpunkten durchgefuehrt
#' und daher auch kein zusaetzliches File eingelesen. Falls solche Differenzen
#' berechnet werden sollen, muss der Time-Lag angegeben werden. Einheiten sind
#' Quartale falls \code{quarter} ungleich NULL, sonst Jahre.
#' @param from Numerischer Vektor mit Jahr und Quartal oder NULL. Falls
#' ungleich NULL, wird hier der Startzeitpunkt uebergeben falls mehr als ein
#' Quartal eingelesen werden soll.
#' @param to Numerischer Vektor mit Jahr und Quartal oder NULL. Falls ungleich
#' NULL, wird hier der Endzeitpunkt uebergeben falls mehr als ein Quartal
#' eingelesen werden soll.
#' @param hh TRUE/FALSE ob auf Haushaltsreferenzpersonen (bstell=0)
#' eingeschraenkt werden soll.
#' @param families TRUE/FALSE ob die 'Stellung zur Familienreferenzperson' auf
#' 'Referenzperson' (xfstell=1) eingeschraenkt werden soll.
#' @param whichVar Character (vector) oder NULL. Falls ungleich NULL, Character Vektor mit Variable(n) aus
#' dem dg7-Mikrozensus-File die im Output-File enthalten sein sollen. Die
#' uebrigen Variablen werden weggelassen. Default ist NULL, dabei werden alle
#' Variablen behalten.
#' @param nbw Numerischer Wert oder NULL. Falls ungleich NULL, Anzahl an Bootstrap-Gewichten die eingelesen
#' werden soll. Default ist NULL, dabei werden alle verfuegbaren
#' Bootstrap-Gewichte eingelesen.
#' @param weightDecimals Numerischer Wert oder NULL. Anzahl der Nachkommastellen der Stichprobengewichte, 
#' gerundet nach SPSS RND Logik (0.5 bwz. -0.5 wird dabei immer "weg von 0" gerundet). 
#' Falls NULL, werden die Gewichte nicht gerundet.
#' @param mz_intern Pfad zu dem `mz_intern` Ordner in der STAT Infrastruktur.
#'   Standardmäßig wird dieser mit `mountSTAT` innerhalb von `sampSTAT` generiert.
#' @return Output ist eine Liste mit einem oder zwei Elementen, je nachdem ob
#' \code{comp_diff_lag=NULL} oder nicht. Die Listenelemente sind Objekte der Klasse data.table.
#' @seealso
#' \code{\link{IndivImportData},\link{GroupSize},\link{GroupRate},\link{Total},\link{Mean},\link{GetLabels},\link{ImportAndMerge},\link{export.mzR}}
#' @export
#' @examples
#' 
#' \dontrun{
#' ## Evt. Memory Limit erhoehen (max bei 32-bit R ist 4095)
#' #memory.limit(size=4095)
#' ### Quartal
#' datq <- ImportData(year=2014, quarter=4)
#' ### Jahr 
#' datj <- ImportData(year=2014)
#' ### Quartal und zugehoeriges Vorjahrsquartal
#' datqdiff <- ImportData(year=2014, quarter=4, comp_diff_lag=4)
#' ### Jahr und Vorjahr
#' datjdiff <- ImportData(year=2014, comp_diff_lag=1)
#' ### Quartal und Vorquartal eingeschraenkt auf Haushaltsreferenzpersonen
#' datqdiffhh <- ImportData(year=2014, quarter=4, comp_diff_lag=1, hh=TRUE)
#' ### Quartal eingeschraenkt auf Familienauswertungsrelevantes
#' datqfam <- ImportData(year=2014, quarter=4, families=TRUE)
#' ### Mehr als ein Jahr einlesen (wegen Memory Limit derzeit begrenzt moeglich
#' ### bzw. sollte 'whichVar' verwendet werden)
#' datzr <- ImportData(from=c(2012,1),to=c(2014,4),
#'   whichVar=c("asbhh","apkz","asbper","ajahr","aquartal","amonat",
#'   "xnuts2","xerwstat"))
#' }
#' 
ImportData <- function(
    year = NULL, quarter = NULL, comp_diff_lag = NULL, from = NULL, to = NULL, 
    hh = FALSE, families = FALSE, whichVar = NULL, nbw = NULL, weightDecimals = 2, 
    mz_intern = mount_mz_intern()
) {
  
  if(!requireNamespace("mountSTAT", quietly = TRUE)){ # TRUE|FALSE) 
    stop("This function can only be used internally at Statistics Austria.\n")
  }
  
  # getFolder()-Funktion einlesen
  source("http://rstudiodevweb.statistik.local/mz/getFolder.R")
  
  
  jahr <- year
  quartal <- quarter
  
  if(!is.null(from) | !is.null(to)){
    if(!is.null(jahr) | !is.null(quartal)){
      cat("Parameter 'jahr' und 'quartal' werden nicht beruecksichtigt wenn 'from' und 'to' spezifiziert wurde.")
    }
    if(any(is.null(from),is.null(to))){
      cat("'from' und 'to' muessen spezifiziert werden um einen Zeitraum einzulesen.")
    }
    ## Sicherheitsabfragen zu from, to, jahr, quartal einbauen.
    jahr <- NULL
    quartal <- NULL
    if(all(from==to)){
      sequence <- format(time(ts(start=from,end=to,frequency=4)))
      jahr_seq <- from[1]
      quartal_seq <- from[2]
    }else{
      sequence <- format(time(ts(start=from,end=to,frequency=4)))
      jahr_seq <- as.numeric(sapply(strsplit(sequence, ".",fixed=TRUE),function(x)x[1]))
      quartal_seq <- as.numeric(plyr::mapvalues(format(sapply(strsplit(sequence, ".",fixed=TRUE),function(x)x[2])), from=c("00","25","50","75"),to=c(1,2,3,4),warn_missing =FALSE))
    }
    
    indatzr <- ImportDataJQ(
      year = jahr_seq[1], quarter = quartal_seq[1], comp_diff_lag =
        comp_diff_lag, hh = hh, families = families, whichVar = whichVar, 
      nbw = nbw, weightDecimals = weightDecimals, mz_intern = mz_intern)  
    
    
    for(i in 2:length(sequence)){
      indat <- ImportDataJQ(
        year = jahr_seq[i], quarter = quartal_seq[i], comp_diff_lag = 
          comp_diff_lag, hh = hh, families = families, whichVar = whichVar, 
        nbw = nbw, weightDecimals = weightDecimals, mz_intern = mz_intern)  
      
      for(j in 1:length(indatzr)){      
        if(length(colnames(indatzr[j][[1]]))>0){
          cn <- intersect(colnames(indatzr[j][[1]]),colnames(indat[j][[1]]))
          
          # ### neue data.table Version hat Probleme beim rbind von Var mit verschiedenen classes.
          # Workaround: (evt bei naechster data.table Vs schon wieder vernachlaessigbar da Bug gefixt sein sollte...)
          if(!identical(lapply(indatzr[j][[1]][,cn,with=FALSE],function(x)class(x)[1]),lapply(indat[j][[1]][,cn,with=FALSE],function(x)class(x)[1]))){
            cn_sel <- names(which(sapply(indatzr[j][[1]][,cn,with=FALSE],function(x)class(x)[1])!=sapply(indat[j][[1]][,cn,with=FALSE],function(x)class(x)[1])))
            indat[j][[1]][,(cn_sel):=lapply(.SD,unclass),.SDcols=cn_sel]
            indatzr[j][[1]][,(cn_sel):=lapply(.SD,unclass),.SDcols=cn_sel]
          }
          
          indatzr[j][[1]] <- rbind(indatzr[j][[1]][,cn,with=FALSE],indat[j][[1]][,cn,with=FALSE],fill=TRUE)
        }else{
          indatzr[j][[1]] <- rbind(indatzr[j][[1]],indat[j][[1]],fill=TRUE)
        }
      }  
      rm(indat);gc()
    }
    for(j in 1:length(indatzr)){ 
      if(is.null(weightDecimals)){
        q_gew <- names(indatzr[j][[1]])[grep("gew1",names(indatzr[j][[1]]))] # will auch bw mitteln
        indatzr[j][[1]] <- indatzr[j][[1]][,(q_gew):=lapply(.SD,function(x){x/length(sequence)}), .SDcols=q_gew]
      }else{#bei STAT-Veroeffentlichungen werden ja Gewichte quasi 2 Mal gerundet. Einmal das gew1 und dann das darauf aufgauende gewjahr nochmal.
        # Quartalsgewichte werden aber in diesen Fall schon bei ImportDataQ bzw dann ImportDataJQ gerundet.
        q_gew <- names(indatzr[j][[1]])[grep("gew1_",names(indatzr[j][[1]]))] ## will bw nicht runden, nur mitteln
        indatzr[j][[1]] <- indatzr[j][[1]][,("gew1"):=lapply(.SD,function(x){round.spss(x/length(sequence),digits=weightDecimals)}), .SDcols="gew1"]
        indatzr[j][[1]] <- indatzr[j][[1]][,(q_gew):=lapply(.SD,function(x){x/length(sequence)}), .SDcols=q_gew]
      }
      names(indatzr)[j] <- paste0("dat_",paste0(from,collapse="q"),"_to_",paste0(to,collapse="q"))
    }
  }else{
    indatzr <- ImportDataJQ(
      year = jahr, quarter = quartal, comp_diff_lag = comp_diff_lag, hh = hh, 
      families = families, whichVar = whichVar, nbw = nbw, 
      weightDecimals = weightDecimals, mz_intern = mz_intern)  
  }   
  
  return(indatzr)  
}

ImportDataQ <- function(
    j, q, comp_jahresgew = FALSE, whichVar = whichVar, hh = hh, families = 
      families, nbw = nbw, weightDecimals = weightDecimals, mz_intern = mz_intern
) {   
  bstell <- xfstell <- asbhh <- NULL #Sonst kommt Fehlermeldung bei Paketbildung: no visible binding for global variable
  
  name_teil <- paste0(j,"q",q)
  dircurrb <- dircurr <- paste0(mz_intern, "/", j, "/", j, "q", q)
  sav_path <- paste0(dircurr,"/dg7.mz",name_teil,".sav")
  
  ##DG7 einlesen
  if(!file.exists(paste0(dircurr,"/dg7.mz",name_teil,".sav"))) {
    
    b_mz <- getFolder("mz")
    dircurr <- paste0(b_mz,  "/01 Datenmanagement/", j, "q", q,"/02 datenbest\U00E4nde")
    sav_path <- paste0(dircurr,"/dg7.mz",name_teil,".sav")
    if(file.exists(sav_path)) {
    warning("Fuer das Quartal ",q," in ",j," gab es noch keine Datenfreigabe!\n",
            "Für UserInnen mit entsprechenden Zugriffsrechten werden jedoch die noch nicht freigegebenen Daten eingelesen.")
    } 
  }
  dat <- data.table(suppressWarnings(spss.get(
    sav_path, use.value.labels = FALSE, allow = FALSE,
    datevars = c("adatum", "adatumpers", "arefwo", "asendf2f", "wvertr", "bgeb", "bgebk", 
                 "boseit", "ckseit", "dseit", "hgefseit", "hseit", "jlwa")
  )))
  cat(dQuote(sav_path), "wurde eingelesen.\n")
  
  if(!is.null(whichVar)){
    dat <- dat[,whichVar,with=F]  
  }
  if(hh){
    if ("bstell" %in% names(dat))
      dat <- dat[bstell==0,]
    else
      dat <- dat[bstell18==0,]
  }
  if(families){
    dat <- dat[xfstell==1,]
  }
  
  ## TODO: Is it a good idea to hardcode the number of bootstrap replicates here?
  if (is.null(nbw))
    nbw = 500
  
  if (nbw > 0){
    bootpath <- paste0(dircurrb, "/mz2_", j, "q", q, "_bootweights.csv.gz")
    
    lfshrb <- fread(input = paste0("zcat ", bootpath), dec = ",", 
                    select = c("asbhh", paste0("gew1_", 1:nbw)), key = "asbhh")
    
    cat(shQuote(bootpath), " wurde eingelesen.\n")    
    
    setkey(dat,asbhh)
    
    if(hh | families){
      dat <- merge(dat,lfshrb,by=c("asbhh"),all.x=TRUE)
    }else{
      dat <- merge(dat,lfshrb,by=c("asbhh"),all=TRUE)  
    }
    
    rm(lfshrb);
    gc()
  }
  
  if(is.null(weightDecimals)){
    if(comp_jahresgew){
      q_gew <- names(dat)[grep("gew1",names(dat))] ## will ja auch die bw mitteln
      dat <- dat[,(q_gew):=lapply(.SD,function(x){x/4}), .SDcols=q_gew]
    }
  }else{
    #q_gew <- names(dat)[grep("gew1",names(dat))] ## will ja auch die bw mitteln und runden
    q_gew <- names(dat)[grep("gew1_",names(dat))] ## will bw NICHT runden
    if(comp_jahresgew){
      dat <- dat[,("gew1"):=lapply(.SD,function(x){round.spss(round.spss(x,digits=weightDecimals)/4,digits=weightDecimals)}), .SDcols="gew1"]
      dat <- dat[,(q_gew):=lapply(.SD,function(x){x/4}), .SDcols=q_gew]
    }else{
      dat <- dat[,("gew1"):=lapply(.SD,function(x){round.spss(x,digits=weightDecimals)}), .SDcols="gew1"]
    }
  }
  
  return(dat)
  
}

ImportDataJQ <- function(
    year, quarter = NULL, comp_diff_lag = NULL, hh = FALSE, families = FALSE, 
    whichVar = NULL, nbw = NULL, weightDecimals = 2, mz_intern
) {
  asbhh <- bstell <- xfstell <- NULL ## initialize to avoid warning
  
  # aus historischen gruenden neue Parameternamen zuweisen, Fkt. wurde urspruenglich mit deutschen Parameternamen geschrieben
  jahr <- year
  quartal <- quarter
  
  # Sichergehen, dass asbper, asbhh etc. bei eingeschraenktem Datensatz dabei sind (braucht man zum Mergen)
  if(!is.null(whichVar)){
    if(!(any(grepl("gew1",whichVar,fixed=TRUE)))){
      whichVar <- c("gew1",whichVar)
    }    
    if(!(any(grepl("asbhh",whichVar,fixed=TRUE)))){
      whichVar <- c("asbhh",whichVar)
    }    
    if(!(any(grepl("asbper",whichVar,fixed=TRUE)))){
      whichVar <- c("asbper",whichVar)
    }
    if(!(any(grepl("apkz",whichVar,fixed=TRUE)))){
      whichVar <- c("apkz",whichVar)
    }
    if(hh && !(any(grepl("bstell",whichVar,fixed=TRUE)))){
      whichVar <- c("bstell",whichVar)
    }
    if(families && !(any(grepl("xfstell",whichVar,fixed=TRUE)))){
      whichVar <- c("xfstell",whichVar)
    }
  }
  
  # vorhandene Quartale
  inp <- vorhQuartaleUndPfade(mz_intern)
  
  # check: es kann nur auf hh ODER families eingeschraenkt werden
  if(hh && families){
    stop("Bitte 'hh' ODER 'families' ausw\u00E4hlen!")
  }
  
  # jahr/quartal vorhanden
  if ( !is.null(quartal) ) {
    if ( !paste0(jahr,"q",quartal) %in% names(inp) ) {
      if(!file.exists(paste0(getFolder("mz"),
                            "/01 Datenmanagement/", jahr, "q", quartal,"/02 datenbest\U00E4nde",
                            "/dg7.mz",paste0(jahr,"q",quartal),".sav"))) {
      stop("fuer das Quartal ",quartal," in ",jahr," gibt es noch keine vollstaendigen Daten!\n")
      }
    }
  } else {
    # 4 Quartale vorhanden
    if ( length(grep(jahr, names(inp))) != 4 ) {
      stop("fuer ",jahr," sind noch nicht alle notwendigen Quartalsergebnisse vorhanden!\n")
    }
  }
  
  # vergangene Zeitraeume vorhanden?
  if ( !is.null(comp_diff_lag) & !is.null(quartal) ) {
    # Fehlerrechnung fuer Differenz zu Vorquartal
    # (oder anderem Quartal mit Time-Lag comp_diff_lag)
    vquartal <- start(lag(ts(start=c(jahr,quartal),frequency=4),k=comp_diff_lag))[2] ## Vorquartal
    vjahr <- start(lag(ts(start=c(jahr,quartal),frequency=4),k=comp_diff_lag))[1] ## Vorjahr
    
    if ( !paste0(vjahr,"q",vquartal) %in% names(inp) ) {
      stop("Fehlerrechnung fuer Differenz zum Vorjahresquartal nicht moeglich.\nDaten fuer Jahr ",vjahr," und Quartal ",vquartal," nicht vorhanden!\n")
    }
  } else if ( !is.null(comp_diff_lag) & is.null(quartal) ) {
    # Fehlerrechnung fuer Differenz zum Vorjahr
    vjahr <- start(lag(ts(start=jahr,frequency=1),k=comp_diff_lag))[1] ## Jahr
    if ( vjahr < 2004 ) {
      stop("Fehlerrechnung fuer Differenz zum Vorjahr nicht moeglich.\nDaten fuer Jahr ",vjahr," nicht vorhanden!\n")
    }
  }
  
  
  
  indat <- list()
  
  if(!is.null(quartal)){
    ##################
    ##    Quartal   ##
    ##################
    
    dat <- ImportDataQ(
      j = jahr, q = quartal, whichVar = whichVar, hh = hh, families = families, 
      nbw = nbw, weightDecimals = weightDecimals, mz_intern = mz_intern)
    indat[[length(indat)+1]] <- dat 
    names(indat)[length(indat)] <- paste0("dat_",jahr,"q",quartal)
    rm(dat);gc()
    
  }else{
    ###############
    ##    Jahr   ##
    ###############
    
    datj <- data.table()
    quartal_orig <- quartal
    
    for(i in 1:4){
      quartal <- i  
      
      dat <- ImportDataQ(
        j = jahr, q = quartal, comp_jahresgew = TRUE, whichVar = whichVar, 
        hh = hh, families = families, nbw = nbw, weightDecimals = 
          weightDecimals, mz_intern = mz_intern)
      
      #if(packageDescription("data.table")$Version)
      if(length(colnames(datj))>0){
        cn <- intersect(colnames(datj),colnames(dat))
        
        # ### neue data.table Version hat Probleme beim rbind von Var mit verschiedenen classes.
        # Workaround: (evt bei naechster data.table Vs schon wieder vernachlaessigbar da Bug gefixt sein sollte...)
        if(!identical(lapply(datj[,cn,with=FALSE],function(x)class(x)[1]),lapply(dat[,cn,with=FALSE],function(x)class(x)[1]))){
          cn_sel <- names(which(sapply(datj[,cn,with=FALSE],function(x)class(x)[1])!=sapply(dat[,cn,with=FALSE],function(x)class(x)[1])))
          dat[,(cn_sel):=lapply(.SD,unclass),.SDcols=cn_sel]
          datj[,(cn_sel):=lapply(.SD,unclass),.SDcols=cn_sel]
        }
        
        datj <- rbind(datj[,cn,with=FALSE],dat[,cn,with=FALSE],fill=TRUE)
        
      }else{
        datj <- rbind(datj,dat,fill=TRUE)
      }
      
      rm(dat);gc()
    }  ##
    
    indat[[length(indat)+1]] <- datj
    names(indat)[length(indat)] <- paste0("dat_",jahr)
    quartal <- quartal_orig
    rm(datj);gc()
  }
  
  if(!is.null(comp_diff_lag) & !is.null(quartal)){
    
    ######################
    ##    Vorquartal    ##
    ######################
    # Fehlerrechnung fuer Differenz zu Vorquartal (oder anderem Quartal mit Time-Lag comp_diff_lag)
    
    vquartal <- start(lag(ts(start=c(jahr,quartal),frequency=4),k=comp_diff_lag))[2] ## Vorquartal
    vjahr <- start(lag(ts(start=c(jahr,quartal),frequency=4),k=comp_diff_lag))[1] ## Vorjahr
    
    datvq <- ImportDataQ(
      j = vjahr, q = vquartal, whichVar = whichVar, hh = hh,
      families = families, nbw = nbw, weightDecimals = weightDecimals,
      mz_intern = mz_intern)
    
    indat[[length(indat)+1]] <- datvq 
    names(indat)[length(indat)] <- paste0("dat_",vjahr,"q",vquartal)
    rm(datvq);gc()
    
  }else if(!is.null(comp_diff_lag) & is.null(quartal)){
    ######################
    ##    Vorjahr    ##
    ######################
    # Fehlerrechnung fuer Differenz zu Vorquartal
    vjahr <- start(lag(ts(start=jahr,frequency=1),k=comp_diff_lag))[1] ## Jahr  
    
    datvj <- data.table()
    
    for(i in 1:4){
      vquartal <- i
      
      datvq <- ImportDataQ(
        j = vjahr, q = vquartal, comp_jahresgew = TRUE, whichVar = whichVar, 
        hh = hh, families = families, nbw = nbw, weightDecimals = 
          weightDecimals, mz_intern = mz_intern)
      
      if(length(colnames(datvj))>0){
        cn <- intersect(colnames(datvj),colnames(datvq))
        
        # ### neue data.table Version hat Probleme beim rbind von Var mit verschiedenen classes.
        # Workaround: (evt bei naechster data.table Vs schon wieder vernachlaessigbar da Bug gefixt sein sollte...)
        if(!identical(lapply(datvj[,cn,with=FALSE],function(x)class(x)[1]),lapply(datvq[,cn,with=FALSE],function(x)class(x)[1]))){
          cn_sel <- names(which(sapply(datvj[,cn,with=FALSE],function(x)class(x)[1])!=sapply(datvq[,cn,with=FALSE],function(x)class(x)[1])))
          datvq[,(cn_sel):=lapply(.SD,unclass),.SDcols=cn_sel]
          datvj[,(cn_sel):=lapply(.SD,unclass),.SDcols=cn_sel]
        }
        
        datvj <- rbind(datvj[,cn,with=FALSE],datvq[,cn,with=FALSE],fill=TRUE)
      }else{
        datvj <- rbind(datvj,datvq,fill=TRUE)
      }
      rm(datvq);gc()
    } 
    
    indat[[length(indat)+1]] <- datvj 
    names(indat)[length(indat)] <- paste0("dat_",vjahr)
    
    rm(datvj);gc()
  }
  
  return(indat)
  
}

