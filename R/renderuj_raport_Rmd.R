renderuj_raport_Rmd <- function(inputFile, clean = FALSE, pdf = TRUE, tex = FALSE, envir = .GlobalEnv){
  fileName = basename(file_path_sans_ext(inputFile))

  intermediateFile = render(
    input = inputFile, #intermediates_dir = paste0(dirname(inputFile), "/", tmpDir),
    output_file = paste0(fileName, ".tex"), encoding = "UTF-8",
    clean = FALSE,
    envir = envir)

  tekstPliku <- paste(readLines(intermediateFile), collapse="\n")

  tekstPliku = gsub("@printLatex\\\\[{](.*?)\\\\[}]",
                    "\n\\1\n",
                    tekstPliku)

  fileConn<-file(intermediateFile)
  writeLines(tekstPliku, fileConn)
  close(fileConn)

  if(pdf){
    oldwd = getwd()
    setwd(dirname(inputFile))
    texi2pdf(intermediateFile, texinputs = getwd())
    setwd(oldwd)
  }

  pliki = list.files(dirname(inputFile), pattern = fileName)

  if(clean){
    pattern = paste0("^", basename(inputFile))
    if(tex) pattern = paste0(pattern, "|^", fileName, ".tex")
    if(pdf) pattern = paste0(pattern, "|^", fileName, ".pdf")
    toDel = pliki[! grepl(pattern, pliki)]
    # file.remove(paste0(dirname(inputFile), "/", toDel))
    if(length(toDel)>0){
      unlink(paste0(dirname(inputFile), "/", toDel), recursive = TRUE)
    }
  }
}
