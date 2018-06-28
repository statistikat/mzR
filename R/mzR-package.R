#' mzR: Microcensus tools
#'
#' R package to compute estimates and their errors for Austrian Microcensus (labour force survey) 
#' data.
#' 
#' @section Test data:
#' [mzTestData]
#' 
#' @section Import of data from STAT internal sources:
#' Import data from the file system provided by STAT. These functions will not be operational for
#' external users.
#' 
#' [ImportData], [ImportDataListQT], [ImportAndMerge]
#' 
#' @section Individual data import:
#' Import spss and csv files based on custom paths.
#' 
#' [IndivImportData]
#' 
#' @section Compute estimates and uncertainty:
#' Create objects of the class `mzR` which contain estimates, confidence intervals and more based
#' on calibrated bootstrapping.
#' 
#' [GroupRate], [GroupSize], [Mean], [Total], [Median]
#' 
#' @section Methods for class `mzR`:
#' [print.mzR], [as.data.frame.mzR], [plot.mzR], [AddVariable], [GetLabels]
#' 
#' @section Calculate tables:
#' Calculate tables for excel export
#' 
#' [MakeTable], [MakeQT], [MakeAKETimeInstantsTable]
#' 
#' @section Excel export:
#' [export], [FillExcelTemplate] 
#'
#' @md
#' @name mzR
#' @docType package
NULL
