#' @title Raport łaczenie
#' @description
#' Funkcja drukuje kod latex raportu z łączenia.
#' @param tabelaPolaczen tabela do wyświetlenia.
#' @param nazwaSkalowania nazwa skalowania, która służy do stworzenia nazw plików wejściowych.
#' @param tabelaSzablonTex ścieżka do szablonu tabeli latex.
#' @param znacznikiTabeli lista ze znacznikami szablonu tabeli.
#' @param tekstAkapitu tekst akapitu pomiędy tabelą i wykresami
#' @param tabelaSzablonRysunkiTex ścieżka do szablonu tabeli, w której znajdą się wykresy
#' @param znacznikiTabeliRysunkow  lista ze znacznikami szablonu tabeli rysunków.
#' @param ktoreRys wektor indeksów określające, które wykresy należy wyświetlić.
#' @param folderWykresow ścieżka do folderów z wykresami.
#' @param szerokosc ułamek określający jaką część strony powinna zajmować tabela z rysunkami.
#' @return Funkcja nie zwraca żadnej wartości.
#' @export
raport_laczenie <- function(tabelaPolaczen, nazwaSkalowania,
                                 tabelaSzablonTex, znacznikiTabeli,
                                 tekstAkapitu,
                                 tabelaSzablonRysunkiTex, znacznikiTabeliRysunkow,
                                 ktoreRys, folderWykresow, szerokosc = 1){

  plikTabeliTex = tabelaSzablonTex
  drukuj_tabele(tabelaPolaczen, tabelaSzablonTex,
                znaczniki = znacznikiTabeli, precyzja = 4)

  if(!is.null(tekstAkapitu)){
    akapit(tekstAkapitu)
  }

  rysunki = paste0(folderWykresow, "{", nazwaSkalowania, "_theta_hist_", ktoreRys, "}.png")

  drukuj_rysunki(rysunki, c(2,2), znacznikiTabeliRysunkow, tabelaSzablonRysunkiTex,
                 szerokosc)
  invisible(TRUE)
}

