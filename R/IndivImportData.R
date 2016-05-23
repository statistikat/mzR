# Output ist eine Liste mit einem oder zwei Elementen, je nachdem ob comp_diff_lag angegeben wurde oder nicht.
#' Mikrozensus-Files und zugehoerige Bootstrapgewichte einlesen.
#' 
#' Funktion liest vom Nutzer zur Verfuegung gestellte Datenfiles ein 
#' 
#' Das mzR-Paket benoetigt in den Daten eine Gewichtsvariable mit dem Namen \code{"gew1"} 
#' und Bootstrapgewichte mit den Namen \code{"gew1_1"}, \code{"gew1_2"}, \code{"gew1_3"}, \ldots (wie beim Mikrozensus ueblich).
#' Sollten diese Variablen in den einzulesenden Daten andere Namen haben, so muessen diese 
#' in den Funktionsparametern \code{weightName} bzw. \code{bwNames} spezifiziert werden.
#' 
#' @param curr_inFile Character oder Character Vektor mit Pfad(en) der
#' MZ-Datei(en) die eingelesen werden soll(en) (bezogen auf den aktuelleren der
#' beiden Zeitpunkte falls prev_inFile ungleich NULL). Eingelesen werden
#' koennen Files vom Typ .sav, .csv und .csv.gz (mit Feldtrennzeichen ; und  Dezimaltrennzeichen ,). Soll ein (Mehr)Jahresdatensatz
#' erstellt werden, muessen mehr Pfade zu den entsprechenden Quartalsfiles
#' uebergeben werden. 
#' @param curr_inFile_bw Character oder Character Vektor mit Pfad(en) der
#' Datei(en) mit den Bootstrapgewichten (File(s) vom Typ .csv und .csv.gz mit Feldtrennzeichen ; und  Dezimaltrennzeichen ,). Soll ein
#' (Mehr)Jahresdatensatz erstellt werden, muessen mehr Pfade zu den
#' entsprechenden Quartalsfiles uebergeben werden.
#' @param prev_inFile Falls ungleich NULL, Pfad(e) der MZ-Datei(en) die
#' eingelesen werden soll(en) (bezogen auf den weniger aktuellen
#' Zeitpunkt). Eingelesen werden koennen Files vom Typ .sav, .csv und .csv.gz (mit Feldtrennzeichen ; und  Dezimaltrennzeichen ,).
#' Soll ein(Mehr)Jahresdatensatz erstellt werden, muessen mehr Pfade zu den
#' entsprechenden Quartalsfiles uebergeben werden. 
#' @param prev_inFile_bw Falls ungleich NULL, character oder character Vektor
#' mit Pfad(en) der Datei(en) mit den Bootstrapgewichten (File(s) vom Typ
#' .csv und .csv.gz mit Feldtrennzeichen ; und  Dezimaltrennzeichen ,) bezogen auf den weniger aktuellen Zeitpunkt. Soll ein
#' (Mehr)Jahresdatensatz erstellt werden, muessen 4 Pfade zu den entsprechenden
#' Quartalsfiles uebergeben werden.
#' @param whichVar Falls ungleich NULL, Character Vektor mit Variable(n) aus
#' dem zur Verfuegung gestellten Datenfile die im Output-File enthalten sein
#' sollen. Die uebrigen Variablen werden weggelassen. Default ist NULL, dabei
#' werden alle Variablen behalten.
#' @param mergeBy Character oder Character Vector der Variablen die zum Mergen von 
#' \code{curr_inFile} und \code{curr_inFile_bw} bzw. \code{prev_inFile} und \code{prev_inFile_bw}
#' verwendet werden sollen. Default ist der Haushalts-Identifikator \code{"asbhh"}.
#' @param nbw Falls ungleich NULL, Anzahl an Bootstrap-Gewichten die eingelesen
#' werden soll. Default ist NULL, dabei werden alle verfuegbaren
#' Bootstrap-Gewichte eingelesen.
#' @param bwNames Falls ungleich NULL, die Variablennamen der Bootstrap-Gewichte 
#' in \code{curr_inFile_bw} bzw. \code{prev_inFile_bw}. Default ist NULL, dabei
#' ist die Variablenbezeichnung der Bootstrapgewichte \code{"gew1_1"}, \code{"gew1_2"}, \code{"gew1_3"}, \ldots wie beim Mikrozensus ueblich.
#' @param weightName Character: Name des Gewichtsvektors der eingelesenen Daten, default ist \code{weightName="gew1"}.
#' @param weightDecimals Numerischer Wert oder NULL. Anzahl der Nachkommastellen der Stichprobengewichte. 
#' Falls NULL, werden die Gewichte so uebernommen wie sie in den eingelesenen Daten enthalten sind.
#' @return Output ist eine Liste mit einem oder zwei Elementen, je nachdem ob
#' \code{prev_inFile=NULL} oder nicht. Die Listenelemente sind Objekte der Klasse data.table.
#' Wurden mehrere Dateipfade angegeben, so enthaelt der Output angepasste Gewichte, 
#' d.h. alle Gewichte werden durch die Anzahl der uebergebenen Dateipfade in \code{curr_inFile} dividiert. 
#' Jahresgewichte berechnen sich also als Quartalsgewichte durch 4.
#' @seealso
#' \code{\link{ImportData},\link{GroupSize},\link{GroupRate},\link{Total},\link{Mean},\link{GetLabels},\link{ImportAndMerge},\link{export}}
#' @export
#' @examples
#' 
#' \dontrun{
#' ## Quartalsdaten
#' curr_inFile <- c("...pfad.../dg8.mz2014Q1.sav")
#' curr_inFile_bw <- c("...pfad.../mz2_2014q1_bootweights.csv.gz")
#' dat <- IndivImportData(curr_inFile=curr_inFile, curr_inFile_bw=curr_inFile_bw)
#' 
#' ## Jahresdaten
#' curr_inFile <- c("...pfad.../dg8.mz2014Q1.sav",
#'                  "...pfad.../dg8.mz2014Q2.sav",
#'                  "...pfad.../dg8.mz2014Q3.sav",
#'                  "...pfad.../dg8.mz2014Q4.sav")
#' curr_inFile_bw <- c("...pfad.../mz2_2014q1_bootweights.csv.gz",
#'                     "...pfad.../mz2_2014q2_bootweights.csv.gz",
#'                     "...pfad.../mz2_2014q3_bootweights.csv.gz",
#'                     "...pfad.../mz2_2014q4_bootweights.csv.gz")
#' dat <- IndivImportData(curr_inFile=curr_inFile, curr_inFile_bw=curr_inFile_bw)
#' 
#' ## Jahresdaten von 2 verschiedenen Zeitpunkten fuer Fehlerrechnung fuer Veraenderungen
#' curr_inFile <- c("...pfad.../dg8.mz2014Q1.sav",
#'                  "...pfad.../dg8.mz2014Q2.sav",
#'                  "...pfad.../dg8.mz2014Q3.sav",
#'                  "...pfad.../dg8.mz2014Q4.sav")
#' curr_inFile_bw <- c("...pfad.../mz2_2014q1_bootweights.csv.gz",
#'                     "...pfad.../mz2_2014q2_bootweights.csv.gz",
#'                     "...pfad.../mz2_2014q3_bootweights.csv.gz",
#'                     "...pfad.../mz2_2014q4_bootweights.csv.gz")
#' prev_inFile <- c("...pfad.../dg8.mz2013Q1.sav",
#'                  "...pfad.../dg8.mz2013Q2.sav",
#'                  "...pfad.../dg8.mz2013Q3.sav",
#'                  "...pfad.../dg8.mz2013Q4.sav")
#' prev_inFile_bw <- c("...pfad.../mz2_2013q1_bootweights.csv.gz",
#'                     "...pfad.../mz2_2013q2_bootweights.csv.gz",
#'                     "...pfad.../mz2_2013q3_bootweights.csv.gz",
#'                     "...pfad.../mz2_2013q4_bootweights.csv.gz")
#' dat <- IndivImportData(curr_inFile=curr_inFile, curr_inFile_bw=curr_inFile_bw,
#'                        prev_inFile=prev_inFile, prev_inFile_bw=prev_inFile_bw)
#' }
#' 
IndivImportData<- function(curr_inFile, curr_inFile_bw, prev_inFile=NULL, prev_inFile_bw=NULL, 
                           whichVar=NULL, mergeBy="asbhh", nbw=NULL, bwNames=NULL, weightName="gew1", weightDecimals=2){
  gc()  
  
  indat_prev <- indat <- list()
  bwNames_orig <- bwNames
  
  if(!is.null(nbw) && !is.null(bwNames)){
    bwNames_orig <- bwNames_orig[1:nbw] 
  }
  
  q_gew <- weightName
  # Sichergehen, dass mergeBy und das Gewicht bei eingeschraenktem Datensatz dabei ist (braucht man ja zum Mergen)
  if(!is.null(whichVar)){
    if(!(all(mergeBy%in%whichVar))){
      whichVar <- c(mergeBy,whichVar[which(!whichVar%in%mergeBy)])
    }    
    if(!(all(weightName%in%whichVar))){
      whichVar <- c(whichVar[which(!whichVar%in%weightName)],weightName)
    }    
  }
  
  
  #########################
  ##    Multiple Files   ##
  #########################
  
  if(length(curr_inFile)>1){
    #cat("Es wurden mehrere Dateipfade angegeben -> ein (Mehr)Jahresdatensatz wird erstellt.\n")
    #cat("Es wurden mehrere Dateipfade angegeben -> Es wird ein Datensatz mit gemittelten Gewichten erstellt.\n")
    #cat("Es wurden mehrere Dateipfade angegeben, daraus wird EIN Datensatz mit angepassten Gewichten erstellt.\n")
    cat("Es wurden mehrere Dateipfade angegeben -> Ein Datensatz mit angepassten Gewichten wird erstellt.\n")
    
    curr_inFile_jahr <- curr_inFile
    curr_inFile_bw_jahr <- curr_inFile_bw
    prev_inFile_jahr <- prev_inFile
    prev_inFile_bw_jahr <- prev_inFile_bw
    
    if(length(curr_inFile_bw)!=length(curr_inFile)){
      stop("Filenamen und Pfade checken!curr_inFile_bw muss gleich viele Dateipfade enthalten wie curr_inFile!\n")
    }
    
    nrMultipleFiles <- length(curr_inFile_jahr)
    
    for(i in 1:nrMultipleFiles){
      curr_inFile <- curr_inFile_jahr[i]
      curr_inFile_bw <- curr_inFile_bw_jahr[i]
      prev_inFile <- prev_inFile_jahr[i]
      prev_inFile_bw <- prev_inFile_bw_jahr[i]
      
      
      ######################
      ##    curr_inFile   ##
      ######################
      
      dat <- IndivImportDataQ(inFile=curr_inFile, inFile_bw=curr_inFile_bw, multipleFiles=TRUE, nrMultipleFiles=nrMultipleFiles, 
                              whichVar=whichVar, mergeBy=mergeBy, nbw=nbw, bwNames=bwNames, weightName=weightName,
                              q_gew=q_gew, bwNames_orig=bwNames_orig, weightDecimals=weightDecimals)
      
      indat[[length(indat)+1]] <- dat 
      
      # if("ajahr"%in%names(dat)){
      #   if("aquartal"%in%names(dat)){
      #     indat_name <- paste0("dat_",unique(dat$ajahr),"q",unique(dat$aquartal))
      #   }else{
      #     indat_name <- paste0("dat_",unique(dat$ajahr))
      #   }
      #   if(length(indat_name)>1){
      #     if(length(unique(dat$ajahr))==1){
      #       indat_name <- paste0("dat_",unique(dat$ajahr))  
      #     }else{
      #       indat_name <- "dat_curr"
      #     }
      #   }
      # }else{
      #   indat_name <- "dat_curr"
      # }
      # 
      # names(indat)[length(indat)] <- indat_name
      rm(dat);gc()
      
      ######################
      ##    prev_inFile   ##
      ######################
      
      if(!is.null(prev_inFile)){
        
        dat <- IndivImportDataQ(inFile=prev_inFile, inFile_bw=prev_inFile_bw, multipleFiles=TRUE, nrMultipleFiles=nrMultipleFiles, 
                                whichVar=whichVar, mergeBy=mergeBy, nbw=nbw, bwNames=bwNames, weightName=weightName,
                                q_gew=q_gew, bwNames_orig=bwNames_orig, weightDecimals=weightDecimals)
        
        indat_prev[[length(indat_prev)+1]] <- dat 
        
        # if("ajahr"%in%names(dat)){
        #   if("aquartal"%in%names(dat)){
        #     indat_name <- paste0("dat_",unique(dat$ajahr),"q",unique(dat$aquartal))
        #   }else{
        #     indat_name <- paste0("dat_",unique(dat$ajahr))
        #   }
        #   if(length(indat_name)>1){
        #     if(length(unique(dat$ajahr))==1){
        #       indat_name <- paste0("dat_",unique(dat$ajahr))  
        #     }else{
        #       indat_name <- "dat_prev"
        #     }
        #   }
        # }else{
        #   indat_name <- "dat_prev"
        # }
        # 
        # names(indat_prev)[length(indat_prev)] <- indat_name
        rm(dat);gc()
        
        
      }
    } ## Ende for-Schleife fuer Einlesen mehrerer Files
    
    ### neue data.table Version kann fill bei rbind nicht mehr fuer class labelled -> Problem abfangen     
    if(!length(unique(sapply(indat,ncol)))==1){
      cn <- unique(unlist(sapply(indat,colnames)))
      cn_info <- unique(unlist(sapply(indat,function(z) cn[which(!cn%in%colnames(z))])))
      indat <- lapply(indat,function(z)
        if(any(cn_info%in%names(z))){
          cn_info_sel <- names(z)[which(names(z)%in%cn_info)]
          z[,(cn_info_sel):=lapply(.SD, unclass),.SDcols=cn_info_sel]
        })
    }
    
    
    if(is.null(prev_inFile)){
      x <- list()
      x[[length(x)+1]] <-  rbindlist(indat,fill=TRUE)
      if("ajahr"%in%names(x[[length(x)]])){
        if(length(unique(x[[length(x)]]$ajahr))==1){
          names(x)[length(x)] <- paste0("j_",unique(x[[length(x)]]$ajahr))  
        }else{
          names(x)[length(x)] <- "dat_curr"
        }
      }else{
        names(x)[length(x)] <- "dat_curr"
      }
      indat <- copy(x)
      rm(x);gc()
    }else{
      ### neue data.table Version kann fill bei rbind nicht mehr fuer class labelled -> Problem abfangen     
      if(!length(unique(sapply(indat_prev,ncol)))==1){
        cn <- unique(unlist(sapply(indat_prev,colnames)))
        cn_info <- unique(unlist(sapply(indat_prev,function(z) cn[which(!cn%in%colnames(z))])))
        indat_prev <- lapply(indat_prev,function(z)
          if(any(cn_info%in%names(z))){
            cn_info_sel <- names(z)[which(names(z)%in%cn_info)]
            z[,(cn_info_sel):=lapply(.SD, unclass),.SDcols=cn_info_sel]
          })
      }
      x <- list()
      x[[length(x)+1]] <-  rbindlist(indat,fill=TRUE)
      if("ajahr"%in%names(x[[length(x)]])){
        if(length(unique(x[[length(x)]]$ajahr))==1){
          names(x)[length(x)] <- paste0("j_",unique(x[[length(x)]]$ajahr))  
        }else{
          names(x)[length(x)] <- "dat_curr"
        }
      }else{
        names(x)[length(x)] <- "dat_curr"
      }
      x[[length(x)+1]] <- rbindlist(indat_prev,fill=TRUE)   
      if("ajahr"%in%names(x[[length(x)]])){
        if(length(unique(x[[length(x)]]$ajahr))==1){
          names(x)[length(x)] <- paste0("j_",unique(x[[length(x)]]$ajahr))  
        }else{
          names(x)[length(x)] <- "dat_prev"
        }
      }else{
        names(x)[length(x)] <- "dat_prev"
      }
      indat <- copy(x)
      rm(x);gc()
    }
    
  }else{
    ##################
    ##    Quartal   ##
    ##################
    
    dat <- IndivImportDataQ(inFile=curr_inFile, inFile_bw = curr_inFile_bw, whichVar=whichVar, mergeBy=mergeBy, nbw=nbw, 
                            bwNames=bwNames, weightName=weightName, q_gew=q_gew, bwNames_orig=bwNames_orig, weightDecimals=weightDecimals)
    
    indat[[length(indat)+1]] <- dat 
    
    if("ajahr"%in%names(dat)){
      if("aquartal"%in%names(dat)){
        indat_name <- paste0("dat_",unique(dat$ajahr),"q",unique(dat$aquartal))
      }else{
        indat_name <- paste0("dat_",unique(dat$ajahr))
      }
      if(length(indat_name)>1){
        if(length(unique(dat$ajahr))==1){
          indat_name <- paste0("dat_",unique(dat$ajahr))  
        }else{
          indat_name <- "dat_curr"
        }
      }
    }else{
      indat_name <- "dat_curr"
    }
    
    names(indat)[length(indat)] <- indat_name
    rm(dat);gc()
    
    ######################
    ##    prev_inFile   ##
    ######################
    
    if(!is.null(prev_inFile)){
      
      dat <- IndivImportDataQ(inFile=prev_inFile, inFile_bw=prev_inFile_bw, whichVar=whichVar, mergeBy=mergeBy, nbw=nbw, 
                              bwNames=bwNames, weightName=weightName, q_gew=q_gew, bwNames_orig=bwNames_orig, weightDecimals=weightDecimals)
      
      indat[[length(indat)+1]] <- dat 
      
      if("ajahr"%in%names(dat)){
        if("aquartal"%in%names(dat)){
          indat_name <- paste0("dat_",unique(dat$ajahr),"q",unique(dat$aquartal))
        }else{
          indat_name <- paste0("dat_",unique(dat$ajahr))
        }
        if(length(indat_name)>1){
          if(length(unique(dat$ajahr))==1){
            indat_name <- paste0("dat_",unique(dat$ajahr))  
          }else{
            indat_name <- "dat_prev"
          }
        }
      }else{
        indat_name <- "dat_prev"
      }
      
      
      names(indat)[length(indat)] <- indat_name
      rm(dat);gc()
      
      
    }
  }  
  
  return(indat)
}

IndivImportDataQ <- function(inFile, inFile_bw, multipleFiles=FALSE, nrMultipleFiles=NULL, whichVar=NULL, mergeBy="asbhh", 
                             nbw=NULL, bwNames=NULL, weightName="gew1", q_gew, bwNames_orig, weightDecimals=2){
  # ######################
  ##    inFile   ##
  ######################
  
  curr_fileType <- unlist(strsplit(inFile,".",fixed=TRUE))[length(unlist(strsplit(inFile,".",fixed=TRUE)))]
  if(curr_fileType=="gz"){
    curr_fileType <- substr(inFile,start=nchar(inFile)-5,stop=nchar(inFile))  
  }
  
  if(!curr_fileType%in%c("sav","csv","csv.gz")){
    stop("Unterst\u00FCtzte Dateiformate: 'sav', 'csv' und 'csv.gz'")
  } 
  
  if(!is.null(whichVar)){
    # evt. noch Warning ausgeben wenn whichVar not in colnames(dat)
    if(curr_fileType=="sav"){
      dat <- data.table(spss.get(grep(inFile,list.files(dirname(inFile),full.names=TRUE),value=TRUE,fixed=TRUE),use.value.labels=FALSE,allow=FALSE)) 
      #if(!is.null(whichVar)){
        dat <- dat[,whichVar,with=F]
     # }
    }else if(curr_fileType=="csv"){
      # kann man evt weglassen, read_csv2(gzfile()) kriegt das auch hin
      dat <- data.table(read_csv2(inFile,n_max=30))  
      whichVar_vec <-sapply(dat,function(x)substr(class(x),start=1,stop=1))  
      whichVar_vec[!names(whichVar_vec)%in%whichVar] <-"_"
      whichVar_vec <- paste(whichVar_vec,collapse="")
      dat <- data.table(read_csv2(inFile,col_types=(whichVar_vec),locale=locale("de")))
    }else if(curr_fileType=="csv.gz"){
      dat <- data.table(read_csv2(gzfile(inFile),n_max=30))  
      whichVar_vec <-sapply(dat,function(x)substr(class(x),start=1,stop=1))  
      whichVar_vec[!names(whichVar_vec)%in%whichVar] <-"_"
      whichVar_vec <- paste(whichVar_vec,collapse="")
      dat <- data.table(read_csv2(gzfile(inFile),col_types=(whichVar_vec),locale=locale("de")))
    }
  }else{
    if(curr_fileType=="sav"){
      dat <- data.table(spss.get(grep(inFile,list.files(dirname(inFile),full.names=TRUE),value=TRUE,fixed=TRUE),use.value.labels=FALSE,allow=FALSE)) 
    }else if(curr_fileType=="csv"){
      dat <- data.table(read_csv2(inFile))  
    }else if(curr_fileType=="csv.gz"){
      dat <- data.table(read_csv2(gzfile(inFile)))
    }
  }
  gc()
  if(!q_gew%in%names(dat)){
    stop("\nGewichtsvariable ",q_gew ," ist im Datensatz nicht vorhanden!\n")
  }
  
  if(q_gew!="gew1"){
    if("gew1"%in%names(dat)){
      newname <- c("gew1_original","gew1_orig","gew1_alt","gew1_kreizbirnbaumhollastaudn")[which(!c("gew1_original","gew1_orig","gew1_alt","gew1_kreizbirnbaumhollastaudn")%in%names(dat))[1]]
      setnames(dat,old="gew1",new=newname)
      warning("\n'gew1' wurde umbenannt zu ",paste0("'",newname,"'"),"! \n")
    }
    setnames(dat,old=q_gew,new="gew1")
    warning("\n",paste0("'",q_gew,"'") ," wurde umbenannt zu 'gew1'!\n")
    
  }
  
  #########################
  ##    inFile_bw   ##
  #########################
  #Bootstrapgewichte einlesen
  dat_bw <- data.table(read_csv2(gzfile(inFile_bw),n_max=1))      
  
  if(any(grepl("gew1_",names(dat_bw))) && is.null(bwNames_orig)){
    bwNames <- grep("gew1_",names(dat_bw),value=TRUE)
  }  
  ## Defaultmaessig nur Quartalsgewichte, also gew1_ einlesen 
  
  #col_types = cols_only("colname" = "?")
  if(is.null(nbw)){
    einlesen <- c(mergeBy,bwNames)
  }else{
    einlesen <- c(mergeBy,bwNames[1:nbw])
  }
  
  col_sel_gew1 <- paste0("cols_only(",paste0("'",einlesen,"'=col_guess()",collapse=","),")")
  
  dat_bw <- data.table(read_csv2(gzfile(inFile_bw),col_types=eval(parse(text=col_sel_gew1)),locale=locale("de")))
  
  ## bw muessen ja in mzR gew1_... heissen
  if(!is.null(bwNames_orig)){
    if(any(grepl("gew1_",names(dat_bw)))){
      oldname <- grep("gew1_",names(dat_bw),value=TRUE)
      newname <- paste0("umbenannt_",1:length(oldname))
      setnames(dat_bw,old=oldname,new=oldname)
      warning("\n",paste0(oldname,collapse=", ")," in '",inFile_bw ,"' wurde umbenannt zu ",paste0(newname,collapse=", "),"!\n")
    }  
    
    setnames(dat_bw,old=bwNames_orig,new=paste0("gew1_",1:length(bwNames_orig)))
    warning("\nbwNames wurden umbenannt zu gew1_1, gew1_2, gew1_3,...\n")
  }
  
  gc()
  
  cat(paste0("'",inFile,"'"), "wurde eingelesen.\n")
  cat(paste0("'",inFile_bw,"'"), "wurde eingelesen.\n") 
  
  if(!all(mergeBy%in%names(dat))){
    stop("\n",paste0(paste0("'",mergeBy,"'"),collapse=" und ")," muss im uebergebenen Datensatz enthalten sein um ihn mit den Bootstrapgewichten mergen zu koennen!\n")
  }
  
  setkeyv(dat,mergeBy)
  setkeyv(dat_bw,mergeBy)
  dat <- merge(dat,dat_bw,by=mergeBy,all.x=TRUE)
  
  # if(multipleFiles){
  #   q_gew_all <- names(dat)[grep("gew1",names(dat))] ## will ja auch die bw mitteln
  #   if(is.null(weightDecimals)){
  #     dat <- dat[,(q_gew_all):=lapply(.SD,function(x){x/nrMultipleFiles}), .SDcols=q_gew_all]
  #   }else{
  #     dat <- dat[,(q_gew_all):=lapply(.SD,function(x){round(x/nrMultipleFiles,digits=weightDecimals)}), .SDcols=q_gew_all]
  #   }
  # }
  
  if(is.null(weightDecimals)){
    if(multipleFiles){
      q_gew_all <- names(dat)[grep("gew1",names(dat))] ## will ja auch die bw mitteln/runden
      dat <- dat[,(q_gew_all):=lapply(.SD,function(x){x/nrMultipleFiles}), .SDcols=q_gew_all]
    }
  }else{
    q_gew_all <- names(dat)[grep("gew1",names(dat))] ## will ja auch die bw mitteln/runden
    if(multipleFiles){
      dat <- dat[,(q_gew_all):=lapply(.SD,function(x){round(x/nrMultipleFiles,digits=weightDecimals)}), .SDcols=q_gew_all]
    }else{
      dat <- dat[,(q_gew_all):=lapply(.SD,function(x){round(x,digits=weightDecimals)}), .SDcols=q_gew_all]
    }
  }
  
  
  rm(dat_bw);gc()
  return(dat)
}
