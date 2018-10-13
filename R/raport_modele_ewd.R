#' @title Drukowanie tabeli
#' @description
#' Funkcja przygotowuje nieco przekazaną jej macierz lub ramkę danych,
#' a następnie zamienia ją na kod Rmarkdown przy pomocy funkcji.
#' \code{\link[knitr]{kable}}
#' @param tab ramka danych lub
#' @param naglowek treść nagłówka
#' @param format.args opcjonalnie list dodatkowych argumentów opisujących
#' formatowanie liczb w tabeli (p. \code{\link[knitr]{kable}})
#' @return
#' Funkcja zwraca kod Rmarkdown, zwrócony przez funkcję
#' \code{\link[knitr]{kable}}.
#' @importFrom knitr kable
#' @export
kable_table = function(tab, naglowek = NULL,
                       format.args = list(decimal.mark = ",", big.mark = " ")){
  tab = as.data.frame(tab)
  rownames(tab) = NULL
  names(tab) = enc2native(names(tab))
  names(tab) = sub("plec", "płeć", names(tab))

  for (i in 1:ncol(tab)) {
    if (all(grepl("TRUE|FALSE", as.character(tab[, i])))) {
      tab[, i] = as.factor(ifelse(grepl("TRUE", tab[, i]), "tak", "nie"))
    }
  }
  classCol = sapply(tab, class)
  align = ifelse(classCol %in% c("numeric", "integer"), "r", "l")
  tab[, classCol == "numeric"] = round(tab[, classCol == "numeric"], 4)
  for (i in which(classCol == "numeric")) {
    # brutalne sprawdzanie dokładności zapisu
    for (j in 0:4) {
      if (all(tab[, i] == round(tab[, i] * 10^j, 0) / 10^j)) {
        break
      }
    }
    tab[, i] = format(tab[, i], nsmall = j, decimal.mark = ",", big.mark = " ")
  }
  kable(tab, align = align, caption = naglowek, format.args = format.args)
}
#' @title Drukowanie tabeli
#' @description
#' Funkcja przygotowuje nieco przekazaną jej macierz lub ramkę danych,
#' a następnie zamienia ją na kod Rmarkdown przy pomocy funkcji.
#' \code{\link[pander]{pander}}
#' @param tab ramka danych lub macierz
#' @param ... opcjonalnie dodatkowe argumenty, które mają zostać przekazane
#' funkcji \code{\link[pander]{pander}}
#' @return
#' Funkcja nic nie zwraca, ale jej wywołanie skutkuje przekazaniem na stdout
#' kodu Rmarkdown wygenerowanego przez funkcję \code{\link[pander]{pander}}.
#' @importFrom pander pander
#' @export
pander_table = function(tab, ...){
  tab = as.data.frame(tab)
  rownames(tab) = NULL
  names(tab) = enc2native(names(tab))
  names(tab) = sub("plec", "płeć", names(tab))

  for (i in 1:ncol(tab)) {
    if (all(grepl("TRUE|FALSE", as.character(tab[, i])))) {
      tab[, i] = as.factor(ifelse(grepl("TRUE", tab[, i]), "tak", "nie"))
    }
  }
  classCol = sapply(tab, class)
  just = ifelse(classCol %in% c("numeric", "integer"), "right", "left")
  tab[, classCol == "numeric"] = round(tab[, classCol == "numeric"], 4)
  for (i in which(classCol == "numeric")) {
    # brutalne sprawdzanie dokładności zapisu
    for (j in 0:4) {
      if (all(tab[, i] == round(tab[, i] * 10^j, 0) / 10^j)) {
        break
      }
    }
    tab[, i] = format(tab[, i], nsmall = j)
  }

  pander(tab, justify = just,  ...)
}
#' @title Naglowek tabeli
#' @description
#' Funkcja generuje kod Rmarkdown opisujący nagłówek tabeli.
#' @param tekst ciąg znaków - opis tabeli
#' @param tabelaStr opcjonalnie ciąg znaków z opisem typu obiektu, którego ma
#' to być nagłówek
#' @return
#' Funkcja zwraca kod Rmarkdown, zwrócony przez funkcję
#' \code{\link[pander]{set.caption}}
#' @importFrom pander set.caption
#' @export
ustaw_naglowek_tabeli = function(tekst, tabelaStr = "Tabela", tryb = "kable"){
  if (tryb == "pander") {
    set.caption(paste0(numeracja(tabelaStr), " ", tekst))
  } else if (tryb == "kable") {
    return(paste0(numeracja(tabelaStr), " ", tekst))
  }
}
#' @title Naglowek wykresu
#' @description
#' Funkcja generuje kod Rmarkdown opisujący nagłówek wykresu.
#' @param tekst ciąg znaków - opis tabeli
#' @param wykresStr opcjonalnie ciąg znaków z opisem typu obiektu, którego ma
#' to być nagłówek
#' @return
#' Funkcja zwraca kod Rmarkdown opisujący nagłówek wykresu
#' @export
daj_naglowek_wykresu = function(tekst, wykresStr = "Wykres"){
  return(paste0(numeracja(wykresStr), " ", tekst))
}
#' @title Wczytywanie wlasnosci modeli
#' @description
#' Funkcja wczytuje plik opisujący własności modeli EWD o różnych stopniach
#' wielomianu opisującego zależność pomiędzy wynikami "na wejściu" a wynikami
#' "na wyjściu".
#' @param plik ciąg znaków - nazwa pliku
#' @param pierwszyWiersz liczba całkowita - od którego wiersza zacząć
#' przetwarzanie pliku
#' @param encoding ciąg znaków określający kodowanie pliku
#' @return
#' Funkcja zwraca listę ramek danych.
#' @importFrom utils type.convert
#' @export
parsuj_plik_porownanie = function(plik, pierwszyWiersz = 1,
                                  encoding = "windows-1250"){
  tekstPliku = readLines(plik, encoding = "windows-1250")
  wiersz = pierwszyWiersz;
  ret = list()

  while (tekstPliku[wiersz] != "") {
    nazwa = tekstPliku[wiersz]
    kolumny = strsplit(tekstPliku[wiersz + 1], "\t")[[1]]
    wiersz = wiersz + 2
    tab = NULL
    while (tekstPliku[wiersz] != "") {
      tmp =  tekstPliku[wiersz]
      tmp = gsub(",", ".", tmp)
      tmp = gsub("\"", "", tmp)
      tab = rbind(tab, strsplit(tmp, split = "\t")[[1]])
      wiersz = wiersz + 1
      if (wiersz > length(tekstPliku)) {
        break
      }
    }
    tab <- as.data.frame(tab, stringsAsFactors = FALSE)
    tab[] <- lapply(tab, type.convert)

    colnames(tab) = kolumny
    ret[[nazwa]] = tab
    wiersz = wiersz + 1
    if (wiersz > length(tekstPliku)) {
      break
    }
  }
  return(ret)
}
#' @importFrom png readPNG writePNG
NULL
