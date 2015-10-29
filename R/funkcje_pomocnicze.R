#' @title Drukowanie tytulu rozdzialu
#' @description
#' Funkcja drukujaca kod latex z tytułem rozdziału.
#' @param tytul tytuł rozdziału.
#' @return
#' Funkcja nic nie zwraca.
#' @export
rozdzial <- function(tytul){
  cat(paste0("\\chapter{", tytul, "}"))
}
#' @title Drukowanie podrozdzialu
#' @description
#' Fukcja drukujaca kod latex z tytułem podrozdziału
#' @param tytul tytuł podrozdziału
#' @return
#' Funkcja nic nie zwraca.
#' @export
podrozdzial <- function(tytul){
  cat(paste0("\\customsect{\\section*{", tytul, "}}"),"\n")
  cat(paste0("\\addcontentsline{toc}{section}{", tytul, "}"))
}
#' @title Drukowanie akapitow
#' @description
#' Funkcja drukuje kod akapitów.
#' @param tresc wektor ciągów znakowych z treścią kolejnych akalpitów.
#' @return
#' Funkcja nic nie zwraca.
#' @export
akapit <- function(tresc){
  cat("\n\n",zamien_znaki_tex(paste(tresc,collapse="\n\n")))
}
#' @title Nowa strona
#' @description
#' Funkcja drukujaca znacznik nowej strony.
#' @return
#' Funkcja nic nie zwraca.
#' @export
nowa_strona <- function(){
  cat("\\newpage")
}
#' @title Lista numerowana
#' @description
#' Funkcja drukuje kod listy numerowanej.
#' @param elementyListy ciągi znakowe będące elementami listy.
#' @return
#' Funkcja nic nie zwraca.
lista_numerowana <- function(elementyListy){
  cat(paste0("\\begin{enumerate}[leftmargin=*]"))
  cat("\n \\item ", paste(elementyListy, collapse="\n \\item "),"\n")
  cat(paste0("\\end{enumerate}"))
}
#' @title Lista punktowana
#' @description
#' Funkcja drukuje kod listy punktowanej
#' @param elementyListy ciągi znakowe będące elementami listy.
#' @return
#' Funkcja nic nie zwraca.
lista_punktowana <- function(elementyListy){
  cat(paste0("\\begin{itemize}[leftmargin=*]"))
  cat("\n \\item ", paste(elementyListy, collapse="\n \\item "),"\n")
  cat(paste0("\\end{itemize} "))
}
#' @title Drukowanie szablonu latex
#' @description
#' Drukuje szablon z pliku oraz zamienia znaczniki na ciągi znakowe zdefiniowane przez użytkownika.
#' @param plikSzablonuTex ścieżka do szablonu.
#' @param znaczniki lista definiująca znaczniki do zabawy.
#' @return
#' Funkcja nic nie zwraca.
#' @export
drukuj_szablon <- function(plikSzablonuTex, znaczniki){

  con = file(plikSzablonuTex, open="r")
  linie = readLines(con)
  close(con)

  for( ind in seq_along(linie) ){
    drukuj_wiersz_tex(linie[ind], znaczniki)
  }
  return(invisible(NULL))
}
#' @title Drukuj wiersz tabeli
#' @description
#' Funkcja drukuje kod pojedynczej tabeli latex'owej.
#' @param linia linia z szablonu tabeli.
#' @param wektor wartości komórek danego wiersza tabeli.
#' @return
#' Funkcja nic nie zwraca.
drukuj_wiersz_tabeli <- function(linia, wektor){
  spRes = strsplit(linia,"(([ ]*)&([ ]*))+")
  if(length(spRes[[1]]) != 2 ){
    stop("Niepoprawna forma wiersza: ", linia)
  }

  cat(spRes[[1]][1], " ", paste0( paste(wektor, collapse=" & "), " ", spRes[[1]][2]), "\n")
  invisible(NULL)
}
#' @title Drukuj wiersz
#' @description
#' Funkcja drukuje wiersz szablonu uwzględniając znacznki.
#' @param linia linia kodu latex.
#' @param znaczniki lista zdefiniowanych znaczników.
#' @return
#' Funkcja nic nie zwraca.
drukuj_wiersz_tex <- function(linia, znaczniki = NULL  ){
  cat(zamien_znaczniki(linia, znaczniki),"\n")
  invisible(NULL)
}
#' @title Drukuj wiersz
#' @description
#' Funkcja zamienia znaczniki na zdefiniowane ciągi znaków.
#' @param nazwa ciąg znaków.
#' @param znaczniki lista znaczników i przypisane im ciągi znakowe.
#' @return
#' Funkcja zwraca ciąg znaków z zamienionymi znacznikami na zdefiniowane
#' w parametrze znaczniki ciągi znaków.
zamien_znaczniki <- function(nazwa, znaczniki ){
  if(is.null(znaczniki)){
    return(nazwa)
  }

  if( !any( mapply(grepl, paste0("!", names(znaczniki)), nazwa) ) ){
    return(nazwa)
  }

  ret = nazwa
  for(i in seq_along(znaczniki)){
    if(grepl("Plik", names(znaczniki)[i])){
      ret = gsub(paste0("!", names(znaczniki)[i]), (znaczniki[[i]]), ret,
                 fixed  = TRUE)
    } else{
      ret = gsub(paste0("!", names(znaczniki)[i]), zamien_znaki_tex(znaczniki[[i]]), ret,
                 fixed  = TRUE)
    }
  }
  return(ret)
}
#' @title Zamien znaki specjalne
#' @description
#' Funkcja przekształca ciąg znaków zastępując wybrane znak specjalne na kod tex, który pozwala
#' na wyświetlenie ich w dokumencie tex.
#' @param nazwa ciąg znaków do zmiany
#' @return Przekształcony ciąg znaków.
zamien_znaki_tex <- function(nazwa){
#   znakiOrg = c("_","^")
#   znakiTex = c("\\_","\\^{ }")
#
#   ret = nazwa
#   for(i in seq_along(znakiOrg)){
#     ret = gsub(znakiOrg[i], znakiTex[i], ret, fixed  = TRUE)
#   }
#
#   return(ret)
  return(zamien_znaki(nazwa, strOrg = c("_","^"), strN = c("\\_","\\^{ }")))
}
#' @title Zamien znaki
#' @description
#' Funkcja przekształca ciąg znaków zastępując podciągi (zadane parametrem znakiOrg)
#' na odpowiadające im podciągi zadane parametrem znakiNowe.
#' @param nazwa ciąg znaków do zmiany
#' @param strOrg wektor ciągów znaków do zastąpienia.
#' @param strN wektor ciągów znaków zastępujące odpowiednie elementy z strOrg.
#' @return Przekształcony ciąg znaków.
#' @export
zamien_znaki <- function(nazwa, strOrg, strN){
  if(length(strOrg)!=length(strN)){
    stop("Różna długość wektorów ze znakami.")
  }

  ret = nazwa
  for(i in seq_along(strOrg)){
    ret = gsub(strOrg[i], strN[i], ret, fixed  = TRUE)
  }

  return(ret)
}
#' @title Inicjalizacja numeracji
#' @description
#' Funkcja inicjalizuje numeracje. Używając numeracji nie powinno się
#' zmieniać zmiennej globalnej określonej parametrem.
#' @param nazwaZmiennejGlobalnej nazwa zmiennej globalnej.
#' @param envir środowisko, gdzie znajduje się zmienna o nazwie nazwaZmiennejGlobalnej.
#' @return
#' Funkcja zwraca obiekt klasy 'NumeracjaTex'.
#' @export
initializuj_numeracje <- function(nazwaZmiennejGlobalnej = "numeracjaTexZG",
                                  envir = .GlobalEnv){
  ret = data.frame(nazwa="Wykresy", numer=0, stringsAsFactors = FALSE)
  class(ret) = "NumeracjaTex"
  # numeracjaTex <<- ret
  assign(nazwaZmiennejGlobalnej, ret, envir = envir)
  invisible(NULL)
}
#' @title Numeracja
#' @description
#' Funkcja dokleja do parametru nazwa numer, który oznacza kolejność wywołania funcji z tym parametrem.
#' Używając numeracji nie powinno się zmieniać zmiennej globalnej 'numeracjaTex'.
#' @param nazwa nazwa numerowanego obiektu, np.: 'Tabela'.
#' @param tylkoNumer jeżeli TRUE zwróci numer obiektu
#' @param nazwaZmiennejGlobalnej nazwa zmiennej globalnej.
#' @param envir środowisko, gdzie znajduje się zmienna o nazwie nazwaZmiennejGlobalnej.
#' @return
#' Funkcja zwraca ciąg znaków.
#' @export
numeracja <- function(nazwa, tylkoNumer = FALSE,
                      nazwaZmiennejGlobalnej = "numeracjaTexZG",
                      envir = .GlobalEnv){
  if( ! nazwaZmiennejGlobalnej %in% ls(envir=envir) || class(get(nazwaZmiennejGlobalnej))!="NumeracjaTex"){
    stop("Obiekt numeracjaTex nie jest poprawnie zainicjowany.")
  }

  numeracjaTex = get(nazwaZmiennejGlobalnej, envir=envir)

  if(!nazwa %in% numeracjaTex$nazwa){
    numeracjaTex <- rbind(numeracjaTex, data.frame(nazwa=nazwa, numer=0, stringsAsFactors = FALSE))
    class(numeracjaTex) = "NumeracjaTex"
    # numeracjaTex <<- ret
    assign(nazwaZmiennejGlobalnej, numeracjaTex)
  }

  numer = numeracjaTex$numer[nazwa==numeracjaTex$nazwa] + 1
  numeracjaTex$numer[nazwa==numeracjaTex$nazwa] <- numer
  assign(nazwaZmiennejGlobalnej, numeracjaTex, envir=envir)
  if(tylkoNumer){
    return(numer)
  } else{
    return(paste0(nazwa, " ", numer, "."))
  }

}
