#' Individuelle Datenfiles zu bereits eingelesenen Mikrozensus-Daten mergen.
#' 
#' Funktion liest vom Nutzer zur Verfuegung gestellte Datenfiles ein und fuehrt
#' diese mit bereits eingelesenen dg7-Mikrozensus-Daten zusammen.
#' 
#' @param x Output-Objekt der Funktion ImportData()
#' @param curr_inFile Pfad der Datei die eingelesen werden soll (bezogen auf
#' den aktuelleren der beiden Zeitpunkte falls prev_inFile ungleich NULL).
#' Eingelesen werden koennen Files vom Typ .sav, .csv und .csv.gz.
#' @param prev_inFile Falls ungleich NULL, Pfad der Datei die eingelesen werden
#' soll (bezogen auf den weniger aktuellen Zeitpunkt). Eingelesen werden koennen
#' Files vom Typ .sav, .csv und .csv.gz.
#' @param mergeBy Character Vektor mit Variable(n) nach denen gemerged werden
#' soll (default="asbper").
#' @param whichVar Falls ungleich NULL, Character Vektor mit Variable(n) aus
#' dem zur Verfuegung gestellten Datenfile die im Output-File enthalten sein
#' sollen. Die ?brigen Variablen werden weggelassen. Default ist NULL, dabei
#' werden alle Variablen behalten.
#' @return Output ist eine Liste mit einem oder zwei Elementen, je nach Laenge
#' des Outputs der Funktion ImportData() .
#' @seealso \code{\link{ImportData}}
#' @export
#' @examples
#' \dontrun{
#' ## Evt. Memory Limit erhoehen (max bei 32-bit R ist 4095)
#' #memory.limit(size=4095)
#' #
#' ### Quartalsdaten
#' #
#' ## dg7-Mikrozensus-Files einlesen
#' dat <- ImportData(year=2014,quarter=4)
#' ## Pfad fuer individuelle Daten festlegen
#' curr_inFile <- "...pfad.../MeineDaten_2014q4.sav"
#' ## Individuelle Daten ueber asbhh und apkz zu bereits eingelesenen 
#' ## dg7-Mikrozensus-Daten mergen
#' dat_indiv <- ImportAndMerge(dat,curr_inFile=curr_inFile,mergeBy=c("asbhh","apkz"))
#' ## dg7-Mikrozensus-Files einlesen: Quartal und zugehoeriges Vorjahrsquartal
#' dat <- ImportData(year=2014,quarter=4, comp_diff_lag=4)
#' ## Pfad fuer individuelle Daten festlegen: aktuelleres Quartal
#' curr_inFile <- "...pfad.../MeineDaten_2014q4.sav"
#' ## Pfad fuer individuelle Daten festlegen: weniger aktuelles Quartal
#' prev_inFile <- "...pfad.../MeineDaten_2013q4.sav"
#' ## Individuelle Daten ueber asbper (default) zu bereits eingelesenen 
#' ## dg7-Mikrozensus-Daten mergen
#' dat_indiv <- ImportAndMerge(dat,curr_inFile=curr_inFile,prev_inFile=prev_inFile)
#' 
#' ### Jahresdaten
#' #
#' ## dg7-Mikrozensus-Jahresdaten einlesen
#' dat <- ImportData(year=2014)
#' curr_inFile <- "...pfad.../MeineDaten_2014j.sav"
#' ## Individuelle Daten ueber asbhh und apkz zu bereits eingelesenen 
#' ## dg7-Mikrozensus-Jahresdaten mergen
#' dat_indiv <- ImportAndMerge(dat, curr_inFile=curr_inFile, mergeBy=c("asbhh","apkz","amonat"))
#' ### dg7-Mikrozensus-Jahresdaten einlesen: Jahr und Vorjahr
#' dat <- ImportData(year=2014, comp_diff_lag=1)
#' ## Pfad fuer individuelle Daten festlegen: aktuelleres Jahr
#' curr_inFile <- "...pfad.../MeineDaten_2014j.sav"
#' ## Pfad fuer individuelle Daten festlegen: weniger aktuelles Jahr
#' prev_inFile  <- "...pfad.../MeineDaten_2013j.sav"
#' ## Individuelle Daten ueber asbper (default) und amonat zu bereits eingelesenen
#' ## dg7-Mikrozensus-Jahresdaten mergen
#' dat_indiv <- ImportAndMerge(dat, curr_inFile=curr_inFile, mergeBy=c("asbper","amonat"))
#' }
#' 
ImportAndMerge <- function(x, curr_inFile, prev_inFile=NULL, mergeBy="asbper", whichVar=NULL){
  curr_inFile <- path.expand(curr_inFile)
  if (!is.null(prev_inFile))
    prev_inFile <- path.expand(prev_inFile)
  gc()
  
  if(length(x)>1 & is.null(prev_inFile)){
    warning("\n!!! Es wurde nur ein File uebergeben:",
            "\ncurr_inFile='",curr_inFile,"'.\n",
            "\nDie Daten mit denen gemerged werden soll enthalten jedoch ",names(x)[1]," und ",names(x)[2],".",
            "\ncurr_inFile wird also nur zu ",  
            names(x)[1], " gespielt. ",
            names(x)[2]," bleibt unveraendert.\n")
  }else if(length(x)==1 & !is.null(prev_inFile)){
    warning("\n!!! Es wurden zwei Files uebergeben: ", 
            "\ncurr_inFile='",curr_inFile,"' und",
            "\nprev_inFile='",prev_inFile,"'.\n",
            "\nDie Daten mit denen gemerged werden soll enthalten jedoch nur ",names(x)[1],".",
            "\ncurr_inFile wird deshalb zu ",  
            names(x)[1], " gespielt. ",
            "\nprev_inFile wird ignoriert.\n")
    prev_inFile <- NULL  
  }
  
  
  # Sichergehen, dass mergeBy-Variable(n) bei eingeschraenktem Datensatz dabei sind  
  if(!is.null(whichVar)){
    if(!all(mergeBy%in%whichVar)){
      whichVar <- c(mergeBy[which(!mergeBy%in%whichVar)],whichVar)
    }
  }
  
  curr_fileType <- tools::file_ext(curr_inFile)
  if(curr_fileType=="gz"){
    curr_fileType <- substr(curr_inFile,start=nchar(curr_inFile)-5,stop=nchar(curr_inFile))  
  }
  
  if(!curr_fileType%in%c("sav","csv","csv.gz")){
    stop("Unterst\u00FCtzte Dateiformate: 'sav', 'csv' und 'csv.gz'")
  } 
  
  if(!is.null(whichVar)){
    # evt. noch Warning ausgeben wenn whichVar not in colnames(dat)
    if(curr_fileType=="sav"){
      dat <- data.table(spss.get(grep(curr_inFile,list.files(dirname(curr_inFile),full.names=TRUE),value=TRUE,fixed=TRUE),use.value.labels=FALSE,allow=FALSE)) 
      if(!is.null(whichVar)){
        dat <- dat[,whichVar,with=F]
      }
    }
    if(curr_fileType=="csv"){
      # evt. fread statt dessen, ist aber derzeit egal
      dat <- data.table(read_csv2(curr_inFile,n_max=30))  
      whichVar_vec <-sapply(dat,function(x)substr(class(x),start=1,stop=1))  
      whichVar_vec[!names(whichVar_vec)%in%whichVar] <-"_"
      whichVar_vec <- paste(whichVar_vec,collapse="")
      dat <- data.table(read_csv2(curr_inFile,col_types=(whichVar_vec)))
    }
    if(curr_fileType=="csv.gz"){
      dat <- data.table(read_csv2(gzfile(curr_inFile),n_max=30))  
      whichVar_vec <-sapply(dat,function(x)substr(class(x),start=1,stop=1))  
      whichVar_vec[!names(whichVar_vec)%in%whichVar] <-"_"
      whichVar_vec <- paste(whichVar_vec,collapse="")
      dat <- data.table(read_csv2(gzfile(curr_inFile),col_types=(whichVar_vec)))
    }
  }else{
    if(curr_fileType=="sav"){
      dat <- data.table(spss.get(grep(curr_inFile,list.files(dirname(curr_inFile),full.names=TRUE),value=TRUE,fixed=TRUE),use.value.labels=FALSE,allow=FALSE)) 
    }
    if(curr_fileType=="csv"){
      dat <- data.table(read_csv2(curr_inFile))  
    }
    if(curr_fileType=="csv.gz"){
      dat <- data.table(read_csv2(gzfile(curr_inFile)))
    }
  }
  gc()
  setkeyv(x[1][[1]],mergeBy)  
  setkeyv(dat,mergeBy)
  x[1][[1]] <- merge(x[1][[1]],dat, all.x=TRUE, suffixes=c("", ".indiv"))
  rm(dat);gc()
  
  cat("\n'",curr_inFile,"'", " wurde dazugespielt.\n",sep="")  
  
  if(!is.null(prev_inFile)){
    prev_fileType <- unlist(strsplit(prev_inFile,".",fixed=TRUE))[length(unlist(strsplit(prev_inFile,".",fixed=TRUE)))]
    if(prev_fileType=="gz"){
      prev_fileType <- substr(prev_inFile,start=nchar(prev_inFile)-5,stop=nchar(prev_inFile))  
    }
    
    if(!prev_fileType%in%c("sav","csv","csv.gz")){
      stop("Unterst\u00FCtzte Dateiformate: 'sav', 'csv' und 'csv.gz'")
    } 
    
    if(!is.null(whichVar)){
      if(prev_fileType=="sav"){
        dat <- data.table(spss.get(grep(prev_inFile,list.files(dirname(prev_inFile),full.names=TRUE),value=TRUE,fixed=TRUE),use.value.labels=FALSE,allow=FALSE)) 
        if(!is.null(whichVar)){
          dat <- dat[,whichVar,with=F]
        }
      }
      if(prev_fileType=="csv"){
        dat <- data.table(read_csv2(prev_inFile,n_max=30))  
        whichVar_vec <-sapply(dat,function(x)substr(class(x),start=1,stop=1))  
        whichVar_vec[!names(whichVar_vec)%in%whichVar] <-"_"
        whichVar_vec <- paste(whichVar_vec,collapse="")
        dat <- data.table(read_csv2(prev_inFile,col_types=(whichVar_vec)))
      }
      if(prev_fileType=="csv.gz"){
        dat <- data.table(read_csv2(gzfile(prev_inFile),n_max=30))  
        whichVar_vec <-sapply(dat,function(x)substr(class(x),start=1,stop=1))  
        whichVar_vec[!names(whichVar_vec)%in%whichVar] <-"_"
        whichVar_vec <- paste(whichVar_vec,collapse="")
        dat <- data.table(read_csv2(gzfile(prev_inFile),col_types=(whichVar_vec)))
      }
    }else{
      if(prev_fileType=="sav"){
        dat <- data.table(spss.get(grep(prev_inFile,list.files(dirname(prev_inFile),full.names=TRUE),value=TRUE,fixed=TRUE),use.value.labels=FALSE,allow=FALSE)) 
      }
      if(prev_fileType=="csv"){
        dat <- data.table(read_csv2(prev_inFile))  
      }
      if(prev_fileType=="csv.gz"){
        dat <- data.table(read_csv2(gzfile(prev_inFile)))
      }
    }
    gc()
    
    setkeyv(x[2][[1]],mergeBy)  
    setkeyv(dat,mergeBy)
    x[2][[1]] <- merge(x[2][[1]],dat, all.x=TRUE, suffixes=c("", ".indiv"))
    rm(dat);gc()    
    
    cat("\n'",prev_inFile,"'", " wurde dazugespielt.\n",sep="")  
  }
  
  return(x)
}

