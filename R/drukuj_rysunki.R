#' @title Drukowanie kodu latex dla tabeli i rysunkĂłw.
#' @description
#' Funkcja drukuje kod latex dla szablinu tabeli z rysunkami zadanymi przez wektor ĹcieĹźek do plikĂłw.
#' @param plikiRys wektor zawierajÄcy ĹcieĹźki do plikĂłw.
#' @param wymiar wymiar tabeli. 
#' @param znacznikiTab lista z argumentami szablonu. 
#' @return 
#' Funkcja nic nie zwraca.
#' drukuj_rysunki(plikiRys = rysynki1,wymiar =  c(4,2), znacznikiTab = znaczTab, 
#' tabelaSzablonTex = tabelaSzablonRysunkiTex)
drukuj_rysunki <- function(plikiRys, wymiar, znacznikiTab, tabelaSzablonTex, szerokosc = 1.0L){
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
