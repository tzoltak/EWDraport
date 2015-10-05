library(EWDraport)
library(knitr)
library(rmarkdown)
library(tools)

plik = "/home/g.golonka/EWDGitHub/EWDraport/JednoroczneGimnazjalne/JednGimn2015.Rmd"

setwd(dirname(plik))
typWskaznika = "TEST jednoroczne"
typSzkoly = "gimnazjum"
rok = 2015
folderWskazniki = "/home/g.golonka/EWDgit/wskGimnJednorocz/kalkulator 2015/"
renderuj_raport_Rmd(plik, TRUE, TRUE, TRUE)
