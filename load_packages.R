load_packages <- function(){
  
  # ezek a packagek kellenek
  packages_to_install <- c("lpSolve", "lpSolveAPI", "latex2exp","ggplot2" )
  #megnézem mik vannak telepítve
  pack<-installed.packages()
  packages<-pack[,1] 
  logical_values <- is.element(packages_to_install, packages)
  #telepítsük a packageket
  if(min(logical_values)==0){
    install.packages(packages_to_install[!logical_values])
  }
  
  # packagek betöltése
  library(lpSolve)
  library(lpSolveAPI)
  library(latex2exp)
  library(ggplot2)
  
  
  # betöltöm a saját függvényeimet
  source("read_input.R") # inputok beolvasása
  source("share_price.R") # részvényárazás
  source("lp_solve.R") # LP megoldó
  source("first_day_pricing.R") # első napi opcióárazó
  source("second_day_pricing.R") # második napi opcióárazó
  source("third_day_pricing.R") # harmadik napi opcióárazó
  source("parameter_sensitivity.R") # ábrákat készít a várható érték és a szórás függvényében
  source("nullTothree.R") # ha nem lehet átrendezni az 1. és 2. időszakban a portfóliót, akkor megmondja az opció árát
  source("nosecondreorder.R") # ha csak az első időszakban lehet átrendezni a portfóliót, de a másodikban nem, akkor megmondja az opció árát
  source("deterministic_share_price.R") # determinisztikus eszközárazás, hogy lefusson 3 időszakra
  
}