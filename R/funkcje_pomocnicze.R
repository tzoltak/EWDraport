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
  cat("\n\n",paste(tresc,collapse="\n\n"))
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
    ret = gsub(paste0("!", names(znaczniki)[i]), znaczniki[[i]], ret, fixed  = TRUE)
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
  znakiOrg = c("_","^")
  znakiTex = c("\\_","\\^{ }")
  
  ret = nazwa
  for(i in seq_along(znakiOrg)){
    ret = gsub(znakiOrg[i], znakiTex[i], ret, fixed  = TRUE)
  }
  
  return(ret)
}