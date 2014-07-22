library(plyr)
library(mirt)
library(EWDskalowanie)
# 
# load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki14_BIC")
# mirtWyniki14_podsum = przygotuj_parametry_2PL(mirtWyniki14)
# save(mirtWyniki14_podsum ,file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki14_podsum")
# 
# load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki03_BIC")
# mirtWyniki03_podsum = przygotuj_parametry_2PL(mirtWyniki03)
# save(mirtWyniki03_podsum ,file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki03_podsum")
# 
# load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_14_BIC")
# mirtWyniki_h2_14_podsum = przygotuj_parametry_2PL(mirtWyniki_h2_14)
# save(mirtWyniki_h2_14_podsum ,file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_14_podsum")
# 
# load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_07_BIC")
# mirtWyniki_h2_07_podsum = przygotuj_parametry_2PL(mirtWyniki_h2_07)
# save(mirtWyniki_h2_07_podsum ,file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_07_podsum")


# obj_podsum = mirtWyniki14_podsum
# ind = 5


rysunki_skalowanie_mirt <-function(obj_podsum, dirPath, nazwaPliku){
  for(ind in 1:length(obj_podsum$theta)){
    theta = obj_podsum$theta[[ind]]
    
    dd = theta[, ncol(theta)]
    png(file = paste0(dirPath, nazwaPliku,"_theta_", ind, ".png"), 
        width = 6, height = 6, units = 'in', res = 500)
    plot(density(dd), main = paste0("Iteracja ", ind), xlab="", ylab="")
    dev.off()
    
    png(file = paste0(dirPath, nazwaPliku,"_theta_hist_", ind, ".png"), 
        width = 6, height = 6, units = 'in', res = 500)
    hist(dd, breaks = seq(min(dd),max(dd),length.out = 1223), main = paste0("Iteracja ", ind), 
         ylab = "", xlab="")
    dev.off()
  }
}

rysunki_skalowanie_polichoryczne <- function(obj_polichoryczny, dirPath, nazwa = "polichor"){
  
  for(numerSkalowania in seq_along(obj_polichoryczny$skalowania) ){
    wynikiSkalowania = obj_polichoryczny$skalowania[[numerSkalowania]]$kalibracja1
    dd = wynikiSkalowania$zapis$gh
    
    if(is.null(dd)){
      break()
    }
    
    png(file = paste0(dirPath, nazwa, "_theta_", numerSkalowania, ".png"), 
        width = 6, height = 6, units = 'in', res = 500)
    plot(density(dd), main = paste0("Iteracja ", numerSkalowania), xlab="", ylab="")
    dev.off()
    
    png(file = paste0(dirPath, nazwa, "_theta_hist_", numerSkalowania, ".png"), 
        width = 6, height = 6, units = 'in', res = 500)
    hist(dd, breaks = seq(min(dd),max(dd),length.out = 1223), main = paste0("Iteracja ", numerSkalowania), 
         ylab = "", xlab="")
    dev.off()
  }
}

dirPath = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/Rysunki/"

load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki14_podsum")
rysunki_skalowanie_mirt(mirtWyniki14_podsum, dirPath, "mirtWyniki14")

load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki03_podsum")
rysunki_skalowanie_mirt(mirtWyniki03_podsum, dirPath, "mirtWyniki03")

load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_14_podsum")
rysunki_skalowanie_mirt(mirtWyniki_h2_14_podsum, dirPath, "mirtWyniki_h2_14")

load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_07_podsum")
rysunki_skalowanie_mirt(mirtWyniki_h2_07_podsum, dirPath, "mirtWyniki_h2_07")

load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/skWynik2008_2")
rysunki_skalowanie_polichoryczne(skWynik2008, dirPath)

load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/skWynik2008_pol")
rysunki_skalowanie_polichoryczne(skWynik2008_pol, dirPath, nazwa = "polichor_pol")


# skWynik2008_pol$skalowania = c(skWynik2008$skalowania[1], skWynik2008_pol$skalowania)
# save(skWynik2008_pol, file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/skWynik2008_pol")
