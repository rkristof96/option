nosecondreorder <- function(step1,input){
  # ha csak két időszak van, akkor az alapján beárazza az opciót
  input_price <- step1$first_day# első időszak részvényárai
  input_onfin <- step1$first_day_onfin #az első időszak önfinanszórozási korlátjának jobb oldala
  first_day_nnp1 <- step1$first_day_non_negativity_part1 # az első időszak nemnegativitási korlátja
  cash_value <- input$cash_value # készpénz értéke
  
  n1 <- input$n1 # első időszakban hány fele igazik a részvény
  n2 <- input$n2 # második időszakban hány fele igazik a részvény
  n3 <- input$n3 # harmadik időszakban hány fele igazik a részvény
  n_of_assets <- input$number_of_assets # hány eszköz van
  
  
  second_day <- share_price(input_price,input$mu,input$sigma, n2) # legenerálja az új részvényárakat és vektorként visszadja
  third_day <- share_price(second_day,input$mu,input$sigma, n3) # legenerálja az új részvényárakat és vektorként visszadja
  # 3 részvény + 1 készpénz van, a készpénz értékét még bele kell rakni a részvények mellé
  third_day_cash <- cbind(rep(cash_value,n3),matrix(third_day,ncol=n_of_assets, nrow=n1*n2*n3, byrow=T))
  # önfinanszírozási korlát jobb oldala: mennyivel ér többet a 2. részvény az 1.-nél
  third_day_onfin <- apply(cbind(third_day_cash[,3]-third_day_cash[,2],0),1,FUN=max)
  third_day_non_negativity <-rep(0,n1*n2*n3)
  
  #negatív előjellel veszük az opció értékét és hozzá pakolom a nullákat
  third_day_b <- c( -input_onfin, -third_day_onfin,third_day_non_negativity)
  # vektort gyártok a kp-val kiegészített részvényárfolyamokból
  third_day_cash_column <- as.vector(t(third_day_cash))
  # relációk vektora
  third_day_relations <-rep(c("=",">="),c(n1+n1*n2*n3,n1*n2*n3))
  # nem negativitási korlát mátrixa
  third_day_non_negativity <- matrix(rep(third_day_cash_column,n1*n2*n3), ncol = n1*n2*n3)
  
  # segédváltozó
  aux_third_day_non_negativity <- matrix(0,nrow=n1*n2*n3, ncol = (n1*(n_of_assets+1)))
  
  
  # a for ciklussal töltöm fel a nemnegativitási korlátot
  for(i in 1:(n1*n2*n3)){
    third_index<-(1+(i-1)*(n_of_assets+1))
    last_index <- i*(n_of_assets+1)
    
    third_day_non_negativity[-(third_index:last_index),i]<-0
    third_index_aux<-(1+((i-1)%/%(n2*n3))*(n_of_assets+1))
    last_index_aux <- (((i-1)%/%(n2*n3)+1)*(n_of_assets+1))
    
    aux_third_day_non_negativity[i,third_index_aux:last_index_aux]<-third_day_non_negativity[(third_index:last_index),i]
  }
  

  first_day_non_negativity_part1_concatenatedzeros <- cbind(first_day_nnp1,matrix(0,nrow=n1, ncol = (n1*n2*n3)*(n_of_assets+1)))
  # önfin egyenletek nullákkal kiegészítve

  
  # önfinanszírozási korlát
  third_day_non_negativity_part1 <- cbind(matrix(0,nrow = n1*n2*n3,ncol = (n_of_assets+1)),-aux_third_day_non_negativity,t(third_day_non_negativity))
  # nemnegativitási korlát
  third_day_non_negativity_part2 <- cbind(matrix(0,ncol=(n_of_assets+1)*(1+n1),nrow=n1*n2*n3),t(third_day_non_negativity))
  # teljes nemnegativitási korlát
  third_day_non_negativity <-rbind( first_day_non_negativity_part1_concatenatedzeros, third_day_non_negativity_part1, third_day_non_negativity_part2)
 # célfüggvény
  objective_function <- c(rep(100,n_of_assets+1),rep(0,(n_of_assets+1)*(n1+n1*n2*n3)))
  #LP megoldása
  third_day_price <-lp_solve("min",objective_function,third_day_non_negativity,third_day_relations,third_day_b)
  
  # output lista
  out <- list()
  
  out[["third_day_price"]] <- third_day_price # ebben minden eredmény benne van
  
  n_dual<- nrow(first_day_nnp1) + nrow(third_day_non_negativity_part1)
  
  out[["payoffs"]] <- c(-input_onfin,-third_day_onfin) # kifizetések
  out[["third_day_dual"]] <- third_day_price$dual[2:(n_dual+1)] # duál változók
  
  return(out)
}