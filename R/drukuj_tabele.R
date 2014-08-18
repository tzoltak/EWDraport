#' @title Tabela latex
#' @description
#' Funkcja przekształca tabelę do kodu tex.
#' @param tabela dane do zaprezentowania w formie tabeli
#' @param plikTabeliTex ścieżka pliku szablonu latex.
#' @param znaczniki lista argumentów, ktore występują w pliku szablonu tabeli.
#' @param ostatniWiersz wektor zawierający ostatni wiersza tabeli.
#' @param precyzja liczba miejsc po przecinku wyświetlanych dla tabeli 
#' @param naglowek zmienna boolowska określająca, czy drukować nagłówek.
#' @param zamien_znaki zmienna określająca, czy zamieniać znaki specjalne latex'owe.
#' @return Funkcja nie zwraca żadnej wartości.
#' @export
drukuj_tabele <- function(tabela, plikTabeliTex, znaczniki = NULL, ostatniWiersz = NULL, precyzja = 3, 
                          naglowek = TRUE, zamien_znaki = TRUE ){
  
  if(!is.data.frame(tabela)){
    tabela = data.frame(tabela, check.names = FALSE)
  }
  
  if( !is.null(ostatniWiersz) &  length(ostatniWiersz) != ncol(tabela)  ){
    stop("Ostani wiersz ma inną liczbę elementów niż wiersze tabeli.")
  }
  
  for(i in 1:ncol(tabela)){
    
    # colnames(tabela) <- zamien_znaki_tex(colnames(tabela))
    
    if(is.factor(tabela[,i])){
      tabela[,i] = as.character(tabela[,i])
    }
    
    if( zamien_znaki & is.character(tabela[,i]) ){
      tabela[,i] = zamien_znaki_tex(tabela[,i])
    }
      
    if(is.numeric(tabela[,i])){
      tabela[,i] = round(tabela[,i], precyzja)
    }
  }
  
  con = file(plikTabeliTex, open="r")
  linie = readLines(con) 
  close(con)
  
  wiersze = which(grepl("&",linie))
  indeks = min(wiersze)-1
  
  for( ind in 1:indeks ){
    drukuj_wiersz_tex(linie[ind], znaczniki)
  }
  
  # drukuj pierwszy wiersz (naglowek tabeli)
  if(naglowek){
    ind =  wiersze[1]
    drukuj_wiersz_tabeli(linie[ind], zamien_znaki_tex(colnames(tabela)))
  }
  
  # drukuj srodkowe wiersze
  srodkowe = linie[wiersze[-c(1,length(wiersze))]]
  srodkoweInd = 1 
  for(kk in 1:nrow(tabela) ){
    drukuj_wiersz_tabeli(srodkowe[srodkoweInd], tabela[kk,])
    
    srodkoweInd = (srodkoweInd + 1) %% length(srodkowe)
    srodkoweInd = ifelse(srodkoweInd == 0, length(srodkowe), srodkoweInd  )
  }
  
  # drukuj ostatni wiersz
  if( !is.null(ostatniWiersz) ){
    ind =  wiersze[length(wiersze)]
    drukuj_wiersz_tabeli(linie[ind], ostatniWiersz )
  }
  
  # drukuj ostanie wiersze szablonu
  indeks = max(wiersze) + 1 
  for( ind in indeks:length(linie) ){
    drukuj_wiersz_tex(linie[ind], znaczniki)
  }
  invisible(NULL)
}