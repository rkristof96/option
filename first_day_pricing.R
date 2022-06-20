first_day_pricing <-function(input,limit_obj_function){
# ha csak egy időszak van, akkor az alapján beárazza az opciót
# de az önfinanszrozási korlátját viszem tovább, ha van második/harmadik időszak
input_price <- input$starting_price # kezdő időszak részvényárai
n1 <- input$n1 # hány részre ágaznak a részvények
n_of_assets <- input$number_of_assets # hány eszköz van
cash_value <- input$cash_value

first_day <- share_price(input_price,input$mu,input$sigma, n1) # legenerálja az új részvényárakat és vektorként visszadja

# 3 részvény + 1 készpénz van, a készpénz értékét még bele kell rakni a részvények mellé
first_day_cash <- cbind(rep(cash_value,n1),matrix(first_day,ncol=n_of_assets, nrow=n1, byrow=T))

# önfinanszírozási korlát jobb oldala: mennyivel ér többet a 2. részvény az 1.-nél
first_day_onfin <-apply(cbind(first_day_cash[,3]-first_day_cash[,2],0),1,FUN=max)

#nem-negativitási korlát
first_day_non_negativity <-rep(0,n1)
#negatív előjellel veszük az opció értékét és hozzá pakolom a nullákat
first_day_b <- c(-first_day_onfin,first_day_non_negativity)
# vektort gyártok a kp-val kiegészített részvényárfolyamokból
first_day_cash_column <- as.vector(t(first_day_cash))
# relációk vektora
first_day_relations <-rep(c("=",">="),c(n1,n1))
# nem negativitási korlát mátrixa
first_day_non_negativity <- matrix(rep(first_day_cash_column,n1), ncol = n1)

# segédváltozó
aux_first_day_non_negativity <-matrix(NA,nrow=n1, ncol = (n_of_assets+1))

# a for ciklussal töltöm fel a nemnegativitási korlátot
for(i in 1:n1){
  first_index<-(1+(i-1)*(n_of_assets+1))
  last_index <- i*(n_of_assets+1)
  
  first_day_non_negativity[-(first_index:last_index),i]<-0
  aux_first_day_non_negativity[i,]<-first_day_non_negativity[(first_index:last_index),i]
}

# önfinanszírozási korlát
first_day_non_negativity_part1 <- cbind(-aux_first_day_non_negativity,t(first_day_non_negativity))
# nem negativitási korlát
first_day_non_negativity_part2 <- cbind(matrix(0,ncol=(n_of_assets+1),nrow=n1),t(first_day_non_negativity))
# önfinanszírozási + nemnegativitási korlát
first_day_non_negativity <-rbind(first_day_non_negativity_part1, first_day_non_negativity_part2)


 #ha a célfüggvény nem korlátos, akkor meg lehet próbálni korlátozni az értékét
if(limit_obj_function==T){

  limit <- c(rep(1,4),rep(0,n1*(n_of_assets)))

  first_day_non_negativity <- rbind(first_day_non_negativity,limit)
  # 100-ban minimálom a célfüggvényt
  first_day_b <- c(first_day_b,0.1)
  first_day_relations <- c(first_day_relations,"=")
}

# célfüggvény
objective_function <- c(rep(100,(n_of_assets+1)),rep(0,(n_of_assets+1)*n1))

#LP megoldása
first_day_price <- lp_solve("min",objective_function,first_day_non_negativity,first_day_relations,first_day_b)


# output lista
out<-list()

out[["first_day_price"]] <- first_day_price # ebben minden eredmény benne van
out[["first_day_obj"]] <- first_day_price$objective # célfüggvény
out[["first_day_dual"]] <- first_day_price$dual[2:(nrow(first_day_non_negativity_part1)+1)]
out[["first_day"]] <- first_day #ebben csak a részvényárak vannak
out[["first_day_onfin"]] <- first_day_onfin # önfinanszírozási korlát jobb oldala
out[["first_day_non_negativity_part1"]] <- first_day_non_negativity_part1 # önfinanszírozási korlát, kell a 2.,3. napi árazáshoz
out[["payoffs"]] <- c(-first_day_onfin) # kifizetések

return(out)
}
