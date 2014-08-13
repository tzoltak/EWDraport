
#' @title Drukowanie szablonu latex
#' @description
#' 
#' @param n
#' @return 
#' @export
rozdzial <- function(tytul){
  cat(paste0("\\chapter{", tytul, "}"))  
}
#' @title Drukowanie szablonu latex
#' @description
#' 
#' @param n
#' @return 
#' @export
podrozdzial <- function(tytul){
  cat(paste0("\\customsect{\\section*{", tytul, "}}"),"\n")
  cat(paste0("\\addcontentsline{toc}{section}{", tytul, "}"))
}

#' @title Drukowanie szablonu latex
#' @description
#' 
#' @param n
#' @return 
#' @export
akapit <- function(tresc){
  cat("\n\n",zamien_znaki_tex(paste(tresc,collapse="\n\n")))
}
#' @title
#' @description
#' 
#' @param n
#' @return 
#' @export
nowa_strona <- function(){
  cat("\\newpage")
}
#' @title
#' @description
#' 
#' @param n
#' @return 
lista_numerowana <- function(elementyListy){
  cat(paste0("\\begin{enumerate}[leftmargin=*]"))
  cat("\n \\item ", paste(elementyListy, collapse="\n \\item "),"\n")
  cat(paste0("\\end{enumerate}")) 
}
#' @title
#' @description
#' 
#' @param n
#' @return 
lista_punktowana <- function(elementyListy){
  cat(paste0("\\begin{itemize}[leftmargin=*]"))
  cat("\n \\item ", paste(elementyListy, collapse="\n \\item "),"\n")
  cat(paste0("\\end{itemize} ")) 
}

#' @title Drukowanie szablonu latex
#' @description
#' 
#' @param n
#' @return 
drukuj_szablon <- function(plikSzablonuTex, znaczniki, tex = TRUE){
  
  con = file(plikSzablonuTex, open="r")
  linie = readLines(con) 
  close(con)
  
  if(tex){
    for( ind in seq_along(linie) ){
      drukuj_wiersz_tex(linie[ind], znaczniki)
    }
    return(invisible(NULL))
  }
  
  ret = ""
  for( ind in seq_along(linie) ){
    ret = paste0(ret, " ", zamien_znaczniki(linie[ind], znaczniki))
  }
  return(ret)
}

#' @title
#' @description
#' 
#' @param n
#' @return 
drukuj_wiersz_tabeli <- function(linia, wektor){
  spRes = strsplit(linia,"(([ ]*)&([ ]*))+")
  if(length(spRes[[1]]) != 2 ){
    stop("Niepoprawna forma wiersza: ", linia)
  }
  
  cat(spRes[[1]][1], " ", paste0( paste(wektor, collapse=" & "), " ", spRes[[1]][2]), "\n")
  invisible(NULL)
}

#' @title
#' @description
#' 
#' @param n
#' @return 
drukuj_wiersz_tex <- function(linia, znaczniki = NULL  ){
  cat(zamien_znaczniki(linia, znaczniki),"\n")
  invisible(NULL)
}

#' @title
#' @description
#' 
#' @param n
#' @return 
zamien_znaczniki <- function(nazwa, znaczniki ){
  if(is.null(znaczniki)){
    return(nazwa)
  }
  
  if( !any( mapply(grepl, paste0("!", names(znaczniki)), nazwa) ) ){
    return(nazwa)
  }
  
  ret = nazwa
  for(i in seq_along(znaczniki)){
    ret = gsub(paste0("!", names(znaczniki)[i]), zamien_znaki_tex(znaczniki[[i]]), ret, fixed  = TRUE)
  }
  return(ret)
}

#' @title Zamień znaki specjalne.
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
#' @title Zamień znaki.
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
#' @title Initializacja numeracji
#' @description
#' Funkcja initializuje numeracje. U?ywaj?c numeracji nie powinno si? 
#' zmienia? zmiennej globalnej 'numeracjaTex'.
#' @return 
#' Funkcja zwraca obiekt klasy 'NumeracjaTex'.
#' @export
initializuj_numeracje <- function(){
  ret = data.frame(nazwa="Wykresy", numer=0, stringsAsFactors = FALSE)
  class(ret) = "NumeracjaTex"
  numeracjaTex <<- ret
  invisible(NULL)
}
#' @title Numeracja
#' @description
#' Funkcja dokleja do parametru nazwa numer, kt?ry oznacza kolejno?? wywo?ania funcji z tym parametrem.
#' U?ywaj?c numeracji nie powinno si? zmienia? zmiennej globalnej 'numeracjaTex'.
#' @param nazwa nazwa numerowanego obiektu
#' @return 
#' @export
numeracja <- function(nazwa){ 
  if( ! "numeracjaTex" %in% ls(envir=.GlobalEnv) || class(get('numeracjaTex'))!="NumeracjaTex"){
    stop("Obiekt numeracjaTex nie jest poprawnie zainicjowany.")
  }
  
  if(!nazwa %in% numeracjaTex$nazwa){
    ret <<- rbind(numeracjaTex, data.frame(nazwa=nazwa, numer=0, stringsAsFactors = FALSE))
    class(ret) = "NumeracjaTex"
    numeracjaTex <<- ret 
  }
  
  numer = numeracjaTex$numer[nazwa==numeracjaTex$nazwa] + 1 
  numeracjaTex$numer[nazwa==numeracjaTex$nazwa] <<- numer
  return(paste0(nazwa, " ", numer, "."))
}


