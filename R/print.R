# Statt "Ver\u00E4nderung" doch besser "Change" in Anzeige
CompFehlerX <- function(x,each=NULL,thousands_separator=TRUE,digits=2){  
  if(is.null(each)){
    
  if(any(grepl("Prev",names(x)))){
    print_obj <- as.data.frame(rbind(
      unlist(x[c("est","sd", "cv", "cil","ciu")] ) ,
      unlist(x[c("estPrev","sdPrev", "cvPrev", "cilPrev","ciuPrev")]),
      unlist(x[c("absChange","sdAbsChange","cvAbsChange","cilAbsChange","ciuAbsChange")]),
      unlist(x[c("relChange","sdRelChange","cvRelChange","cilRelChange","ciuRelChange")])
    ))
    print_obj <- round(print_obj,digits=digits)
    if(thousands_separator){
    print_obj <- format(print_obj, big.mark = ",")
    }
    row.names(print_obj) <- c(x[["date"]],x[["datePrev"]],"Absolute change","Relative change")
    colnames(print_obj) <- c("est","sd", "cv",paste(names(x[c("cil","ciu")]),unlist(lapply(x[c("cil","ciu")],names)),sep="_"))
  }else{
    print_obj <- as.data.frame(x[c("est","sd", "cv", "cil","ciu")])
    print_obj <- round(print_obj,digits=digits)
    if(thousands_separator){
      print_obj <- format(print_obj, big.mark = ",")
    }
    row.names(print_obj) <- x[["date"]]
    colnames(print_obj) <- c("est","sd", "cv",paste(names(x[c("cil","ciu")]),unlist(lapply(x[c("cil","ciu")],names)),sep="_"))
  }
  
  print(print_obj)  
  
  }else{
    
    if(any(grepl("Prev",names(x[[1]])))){
      print_obj_list <- list()
      for(i in 1:length(x)){
      print_obj <- as.data.frame(rbind(
        unlist(x[[i]][c("est","sd", "cv", "cil","ciu")] ) ,
        unlist(x[[i]][c("estPrev","sdPrev", "cvPrev", "cilPrev","ciuPrev")]),
        unlist(x[[i]][c("absChange","sdAbsChange","cvAbsChange","cilAbsChange","ciuAbsChange")]),
        unlist(x[[i]][c("relChange","sdRelChange","cvRelChange","cilRelChange","ciuRelChange")])
      ))
      print_obj <- round(print_obj,digits=digits)
      if(thousands_separator){
        print_obj <- format(print_obj, big.mark = ",")
      }
      row.names(print_obj) <- c(x[[i]][["date"]],x[[i]][["datePrev"]],"Absolute change","Relative change")
      colnames(print_obj) <- c("est","sd", "cv",paste(names(x[[i]][c("cil","ciu")]),unlist(lapply(x[[i]][c("cil","ciu")],names)),sep="_"))
      print_obj_list[[length(print_obj_list)+1]] <- print_obj
      names(print_obj_list)[i] <- names(x)[i]
      
      }
      #do.call(rbind,rbind(print_obj_list,rep("",5)))      
      }else{      
      print_obj_list <- list()
      for(i in 1:length(x)){
      print_obj<- as.data.frame(x[[i]][c("est","sd", "cv", "cil","ciu")])
      print_obj <- round(print_obj,digits=digits)
      if(thousands_separator){
        print_obj <- format(print_obj, big.mark = ",")
      }
      row.names(print_obj) <- x[[i]][["date"]]
      colnames(print_obj) <- c("est","sd", "cv",paste(names(x[[i]][c("cil","ciu")]),unlist(lapply(x[[i]][c("cil","ciu")],names)),sep="_"))
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
