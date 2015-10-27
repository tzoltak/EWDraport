#' @title Funkcja renderująca plik szablonu RMD.
#' @description
#' Funkcja służy do renderowania plików rmd na formaty: pdfi tex.
#' @param inputFile nazwa pliku rmd z szablonem raportu.
#' @param clean jeżeli ustawiony na TRUE to usuwane są wszystkie pliki robocze.
#' (UWAGA: Mogą zostać usunięte inne pliki, których nazwy na początku zawierają nazwę szablonu).
#' @param pdf jeżeli ustawiony na TRUE to generuje plik pdf.
#' @param tex jeżeli ustawiony na TRUE to generuje plik tex.
#' @param envir środowisko renderowania szablonu rmd. Powinno zawierać wszystkie stałe globalne, które używane są w szablonie
#' @return
#' Funkcja nic nie zwraca.
#' @import tools
#' @import rmarkdown
#' @export
renderuj_raport_Rmd <- function(inputFile, clean = FALSE, pdf = TRUE, tex = FALSE, envir = .GlobalEnv){
  fileName = basename(file_path_sans_ext(inputFile))

  intermediateFile = render(
    input = inputFile, #intermediates_dir = paste0(dirname(inputFile), "/", tmpDir),
    output_file = paste0(fileName, ".tex"), encoding = "UTF-8",
    clean = FALSE,
    envir = envir)

  #tekstPliku <- paste(readLines(intermediateFile), collapse="\n")

  #tekstPliku = gsub("@printLatex\\\\[{](.*?)\\\\[}]",
  #                  "\n\\1\n",
  #                  tekstPliku)

  #fileConn<-file(intermediateFile)
  #writeLines(tekstPliku, fileConn)
  #close(fileConn)

  if(pdf){
    oldwd = getwd()
    setwd(dirname(inputFile))
    texi2pdf(basename(intermediateFile), texinputs = getwd())
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
  return(invisible(NULL))
}
