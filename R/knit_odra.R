#' @title Knit Odra
#' @description
#' Funkcja służąca do budowania raportów za pomocą funkcji \link{knit} na serwerze Odra.
#' Wprowadzona w celu generowania raportów z poprawnymi polskimi znakami.
#' @param raport nazwa pliku knitr do wygenerowania raportu.
# @param roboczy ścieżka do folderu zawierającego plik 'raport'.
#' @return 
#' Funkcja nic nie zwraca.
#' @export
knit_odra <- function(raport, folder){
  
  if(!grepl(".Rnw$", raport)){
    stop("Niepoprawne rozszerzenie pliku.")
  }
  

  tryCatch({
    starywd = getwd()
    fpFile = paste0(starywd, "/", raport)
    dirName = dirname(fpFile)
    
    if (!file.exists(fpFile)){
      stop("Podano błędny plik: \n", fpFile)
    } else if(dirName!=starywd ){
      setwd(dirName)
    }
    
    texFile = paste0(strsplit(fpFile,".Rnw")[[1]], ".tex")
    knit(fpFile, output=texFile, envir =.GlobalEnv)
    
    system(paste0("pdflatex ", texFile))
  }, warning = function(w) {
    warning(w)
  }, error = function(e) {
    stop(e)
  }, finally = {
    setwd(starywd)
  })
  invisible(TRUE)
}


