#' @title Knit Odra
#' @description
#' Funkcja służąca do budowania raportów za pomocą funkcji \link{knit} na serwerze Odra.
#' Wprowadzona w celu generowania raportów z poprawnymi polskimi znakami.
#' @param file ścieżka względna do plik z rozszerzeniem 'Rnw'.
#' @return 
#' Funkcja nic nie zwraca.
#' @export
knit_odra <- function(file){
  
  if(!grepl(".Rnw$", file)){
    stop("Niepoprawne rozszerzenie pliku.")
  }
  starywd = getwd()
  fpFile = paste0(starywd, "/", file)
  dirName = dirname(fpFile)
  
  if (!file.exists(fpFile)){
    stop("Podano błędny plik: \n", fpFile)
  } else if(dirName!=starywd ){
    setwd(dirName)
  }
  
  texFile = paste0(strsplit(fpFile,".Rnw")[[1]], ".tex")
  knit(fpFile, output=texFile)
  
  system(paste0("pdflatex ", texFile))
  setwd(starywd)
  invisible(TRUE)
}


