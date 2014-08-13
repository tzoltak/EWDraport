rm(list = ls())
library(EWDraport)
library(knitr)
knit_moj("RaportCzesciEgzaminow.Rnw", 
         roboczy = "/home/g.golonka/EWDgit/EWDraport/LaczenieCzesciEgzaminow/")



# for f in *\ *; do mv "$f" "${f// /_}"; done
# for f in *-*; do mv "$f" "${f//-/_}"; done

# knit_moj("RaportLaczenie.Rnw", 
#          roboczy = "/home/g.golonka/EWDgit/EWDraport/LaczenieKryteriow/")

rm(list = ls())
folderPolichoryczne= "/home/g.golonka/daneEgzaminy/laczenie/korelacjePoli/"
folderPolichoryczneWykresy = "/home/g.golonka/daneEgzaminy/laczenie/korelacjePoli/wykresy/"
pliki = list.files(folderPolichoryczne, pattern =".csv.rkp$")

ret = list()
str = NULL
for(plik_param in pliki){
  message(plik_param)
  a = knit_moj("RaportCzesciEgzaminow.Rnw", 
               roboczy = "/home/g.golonka/EWDgit/EWDraport/LaczenieCzesciEgzaminow/")
  
  message(plik_param)
}













