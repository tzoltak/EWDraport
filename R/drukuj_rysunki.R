#' @title Drukowanie kodu latex dla tabeli i rysunkow
#' @description
#' Funkcja drukuje kod latex dla szablinu tabeli z rysunkami zadanymi przez wektor ścieżek do plików.
#' @param plikiRys wektor zawierający ścieżki do plików.
#' @param wymiar wymiar tabeli.
#' @param znacznikiTab lista z argumentami szablonu.
#' @param tabelaSzablonTex ścieżka do pliku z szablonem tabeli.
#' @param szerokosc szerokość tabeli z rysunkami. Jeden oznacza całą szerokość strony.
#' @return
#' Funkcja nic nie zwraca.
#' @export
drukuj_rysunki <- function(plikiRys, wymiar, znacznikiTab, tabelaSzablonTex, szerokosc = 1L){
  tab = array("",wymiar)
  for (k in 1:wymiar[1]){
    for (n in 1:wymiar[2]){

      if((k-1)*wymiar[2]+n > length(plikiRys)){
        break;
      }

      # \includegraphics[width=\textwidth*\real{0.45}]{pic}
      tab[k,n] =  paste0("\\includegraphics[width=\\textwidth*\\real{",szerokosc/wymiar[1],"}]{",
                         plikiRys[(k-1)*wymiar[2]+n],
                          "}"
                        )
    }
  }
  drukuj_tabele (tab, tabelaSzablonTex, znacznikiTab, naglowek = FALSE, zamien_znaki = FALSE)
  invisible(NULL)
}
