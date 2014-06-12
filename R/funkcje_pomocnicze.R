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