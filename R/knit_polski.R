knit_moj <- function(file){
  
  if(!grepl(".Rnw$", file)){
    stop("Niepoprawne rozszerzenie pliku.")
  }
  fpFile = paste0(starywd <- getwd(),"/", file)
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
}


