second_day_pricing <- function(step1,input){
# ha csak két időszak van, akkor az alapján beárazza az opciót
# de az önfinanszrozási korlátját viszem tovább, ha van harmadik időszak
input_price <- step1$first_day # első időszak részvényárai
input_onfin <- step1$first_day_onfin #az első időszak önfinanszórozási korlátjának jobb oldala
cash_value <- input$cash_value # készpénz értéke
first_day_nnp1 <- step1$first_day_non_negativity_part1 # az első időszak nemnegativitási korlátja

n1 <- input$n1 # első időszakban hány fele igazik a részvény
n2 <- input$n2 # második időszakban hány fele igazik a részvény
n_of_assets <- input$number_of_assets # hány eszköz van


second_day <- share_price(input_price,input$mu,input$sigma, n2) # legenerálja az új részvényárakat és vektorként visszadja
# 3 részvény + 1 készpénz van, a készpénz értékét még bele kell rakni a részvények mellé
second_day_cash <- cbind(rep(cash_value,n2),matrix(second_day,ncol=n_of_assets, nrow=n1*n2, byrow=T))
# önfinanszírozási korlát jobb oldala: mennyivel ér többet a 2. részvény az 1.-nél
second_day_onfin <-apply(cbind(second_day_cash[,3]-second_day_cash[,2],0),1,FUN=max)
second_day_non_negativity <-rep(0,n1*n2)


#negatív előjellel veszük az opció értékét és hozzá pakolom a nullákat
second_day_b <- c(-input_onfin,-second_day_onfin,second_day_non_negativity)
# vektort gyártok a kp-val kiegészített részvényárfolyamokból
second_day_cash_column <- as.vector(t(second_day_cash))
# relációk vektora
second_day_relations <-rep(c("=",">="),c(n1+n1*n2,n1*n2))
# nem negativitási korlát mátrixa
second_day_non_negativity <- matrix(rep(second_day_cash_column,n1*n2), ncol = n1*n2)
# segédváltozó
aux_second_day_non_negativity <-matrix(0,nrow=n1*n2, ncol = (n_of_assets+1)*n1)

# a for ciklussal töltöm fel a nemnegativitási korlátot
for(i in 1:(n1*n2)){
  second_index<-(1+(i-1)*(n_of_assets+1))
  last_index <- i*(n_of_assets+1)
  
  second_day_non_negativity[-(second_index:last_index),i]<-0
  
  second_index_aux<-(1+((i-1)%/%n2)*(n_of_assets+1))
  last_index_aux <- (((i-1)%/%n2+1)*(n_of_assets+1))
  
  aux_second_day_non_negativity[i,second_index_aux:last_index_aux]<-second_day_non_negativity[(second_index:last_index),i]
}

first_day_non_negativity_part1_concatenatedzeros <- cbind(first_day_nnp1,matrix(0,nrow=n1, ncol = n1*n2*(n_of_assets+1)))


# önfinanszírozási korlát
second_day_non_negativity_part1 <- cbind(matrix(0,nrow=n1*n2, ncol = (n_of_assets+1)),-aux_second_day_non_negativity,t(second_day_non_negativity))
# nem negativitási korlát
second_day_non_negativity_part2 <- cbind(matrix(0,ncol=(n_of_assets+1)*(1+n1),nrow=n1*n2),t(second_day_non_negativity))

# önfinanszírozási + nemnegativitási korlát
second_day_non_negativity <-rbind(second_day_non_negativity_part1, second_day_non_negativity_part2)
# összefűzöm az összes korlátot
second_day_non_negativity <- rbind(first_day_non_negativity_part1_concatenatedzeros,second_day_non_negativity)

# célfüggvény
objective_function <- c(rep(100,n_of_assets+1),rep(0,(n_of_assets+1)*(n1+n1*n2)))

#LP megoldása
second_day_price <-lp_solve("min",objective_function,second_day_non_negativity,second_day_relations,second_day_b)

# output lista
out<-list()

out[["second_day_price"]] <- second_day_price  # ebben minden eredmény benne van
out[["second_day"]] <- second_day #ebben csak a részvényárak vannak
out[["second_day_onfin"]] <- second_day_onfin  # önfinanszírozási korlát jobb oldala
out[["second_day_non_negativity_part1"]] <- second_day_non_negativity_part1 # önfinanszírozási korlát, kell a 3. napi árazáshoz

n_dual<- dim(first_day_nnp1)[1]+nrow(second_day_non_negativity_part1)

out[["second_day_dual"]] <- second_day_price$dual[2:(n_dual+1)] # duál változók
out[["payoffs"]] <- c(-input_onfin,-second_day_onfin) # kifizetések


return(out)
}
