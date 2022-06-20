share_price<-function(input_price,lmu, lsigma ,number_of_scenarios){
  # eza  függvény árazza be a részvényeket a t-1 időszakról a t.-re
  
  # lognormális eloszlás paraméterei
  mu=lmu
  sigma=lsigma
  # input_price sorai
  rows=length(input_price)
  
  # zaj generálás
  noise <- rlnorm(number_of_scenarios*rows, meanlog = mu, sdlog = sigma)
  # eltoláshoz használt konstans
  noise_plus_const <- noise + 0.5*100 - exp(2.5) # konstans értékeket külön veszem
  
  # zajból lesz mátrix
  outmatrix <- matrix(noise_plus_const,nrow = number_of_scenarios, ncol = rows)
  
  # előző érték (t-1.) felét is hozzáadjuk
  backshift <- input_price*0.5
  # for ciklus nélkül is hozzá lehet adni a backshiftet a mátrixos megoldással
  # az előző érték + a zaj + konstans értékek összeadása
  output <- outmatrix + backshift
  # de nem ilyen formátumban kellene, hanem vektorként soronként kibontva
  
  out <-  as.vector(t(output))
  # felbontja a vektort
  
  return(out)
}
