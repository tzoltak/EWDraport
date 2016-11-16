#' @title Funkcja renderujaca plik szablonu RMD
#' @description
#' Funkcja służy do renderowania raportów na podstawie szablonów zapisanych
#' w plikach .Rmd. Jeśli w ramach pakietu został zdefiniowany plik
#' z szablonem o nazwie takiej, jak podana argumentem \code{szablon}, zostanie
#' on wczytany z zasobów pakietu. W przeciwnym wypadku, argument \code{szablon}
#' zostanie potraktowany jako ścieżka do pliku z szablonem względem aktywnego
#' katalogu roboczego.
#' @param szablon nazwa pliku z szablonem raportu w formacie .Rmd
#' @param nazwaPliku nazwa pliku wynikowego (razem z rozszerzeniem określającym
#' format)
#' @param obiekty lista obiektów, z których korzysta szablon do wygenerowania
#' raportu
#' @param clean wartość logiczna - czy usunąć wszystkie pliki robocze generowane
#' przez pander przy tworzeniu pliku wynikowego?
#' @return
#' Funkcja nic nie zwraca.
#' @import rmarkdown
#' @importFrom utils installed.packages
#' @export
renderuj_raport_Rmd = function(szablon, nazwaPliku, obiekty = list(),
                               clean = TRUE) {
  stopifnot(is.character(szablon), length(szablon) == 1,
            is.character(nazwaPliku), length(nazwaPliku) == 1,
            is.list(obiekty),
            is.vector(clean), length(clean) == 1)
  stopifnot(clean %in% c(TRUE, FALSE))
  if (length(obiekty) > 0) {
    stopifnot(!is.null(names(obiekty)))
  }
  folderRoboczy = getwd()

  szablon = paste0(sub("[.]Rmd$", "", szablon), ".Rmd")
  temp = installed.packages()[, 1:2]
  temp = paste0(temp[temp[, 1] == "EWDraport", 2], "/EWDraport/", szablon)
  if (file.exists(temp)) {
    szablon = temp
  } else if (!file.exists(szablon)) {
    stop("Nie znaleziono podanego pliku z szablonem raportu.")
  }

  render(input = szablon, output_file = paste0(folderRoboczy, "/", nazwaPliku),
         encoding = "UTF-8", clean = clean)
  temp = sub("([^.])[.][^.]+$", "\\1.tex", nazwaPliku)
  if (clean & file.exists(temp)) {
    unlink(temp)
  }
  return(invisible(NULL))
}
