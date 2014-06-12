#' @title Drukowanie kodu latex dla tabeli i rysunków.
#' @description
#' Funkcja drukuje kod latex dla szablinu tabeli z rysunkami zadanymi przez wektor ścieżek do plików.
#' @param plikiRys wektor zawierający ścieżki do plików.
#' @param wymiar wymiar tabeli. 
#' @param znacznikiTab lista z argumentami szablonu. 
#' @return 
#' Funkcja nic nie zwraca.
drukuj_rysunki <- function(plikiRys, wymiar, znacznikiTab, tabelaSzablonTex){
  tab = array("",wymiar)
  for (k in 1:wymiar[1]){
    for (n in 1:wymiar[2]){
      tab[k,n] =  paste0("\\includegraphics[width=\\textwidth/",wymiar[1],"]{",
                         plikiRys[(k-1)*wymiar[1]+n],
                          "}"
                        )
    }
  }
  drukuj_tabele (tab, tabelaSzablonTex, znacznikiTab, naglowek = FALSE, zamien_znaki = FALSE)
  invisible(NULL)
}
