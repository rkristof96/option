nullTothree <- function(input){
  
  # ha három időszak van, akkor az alapján beárazza az opciót
  n1 <- input$n1 # első időszakban hány fele igazik a részvény
  n2 <- input$n2 # második időszakban hány fele igazik a részvény
  n3 <- input$n3 # harmadik időszakban hány fele igazik a részvény
  cash_value <- input$cash_value # készpénz értéke
  n_of_assets <- input$number_of_assets # hány eszköz van
  starting_price <- input$starting_price
  
  first_day <- share_price(starting_price,input$mu,input$sigma, n1) # legenerálja az új részvényárakat és vektorként visszadja
  second_day <- share_price(first_day,input$mu,input$sigma, n2)
  third_day <- share_price(second_day,input$mu,input$sigma, n3)
  
  third_day_cash <- cbind(rep(cash_value,n3),matrix(third_day,ncol=n_of_assets, nrow=n1*n2*n3, byrow=T))
  # önfinanszírozási korlát jobb oldala: mennyivel ér többet a 2. részvény az 1.-nél
  third_day_onfin <- apply(cbind(third_day_cash[,3]-third_day_cash[,2],0),1,FUN=max)
  third_day_non_negativity <-rep(0,n1*n2*n3)
  
  third_day_b <- c(-third_day_onfin,third_day_non_negativity)
  # vektort gyártok a kp-val kiegészített részvényárfolyamokból
  third_day_cash_column <- as.vector(t(third_day_cash))
  # relációk vektora
  third_day_relations <-rep(c("=",">="),c(n1*n2*n3,n1*n2*n3))
  # nem negativitási korlát mátrixa
  third_day_non_negativity <- matrix(rep(third_day_cash_column,n1*n2*n3), ncol = n1*n2*n3)
  
  # segédváltozó
  aux_third_day_non_negativity <- matrix(0,nrow=n1*n2*n3, ncol = (n_of_assets+1))
  
  # a for ciklussal töltöm fel a nemnegativitási korlátot
  for(i in 1:(n1*n2*n3)){
    third_index<-(1+(i-1)*(n_of_assets+1))
    last_index <- i*(n_of_assets+1)
    
    third_day_non_negativity[-(third_index:last_index),i]<-0
    aux_third_day_non_negativity[i,] <- -third_day_non_negativity[(third_index:last_index),i]
  }
  # önfinanszírozási korlát
  third_day_non_negativity1 <-cbind(aux_third_day_non_negativity,t(third_day_non_negativity))
  # nemnegativitási korlát
  third_day_non_negativity2 <- cbind(matrix(0,ncol = (n_of_assets+1), nrow = n1*n2*n3),t(third_day_non_negativity))
  # összefűzzük
  third_day_non_negativity <-rbind(third_day_non_negativity1,third_day_non_negativity2)
  # célfüggvény
  objective_function <- c(rep(100,n_of_assets+1),rep(0,(n_of_assets+1)*(n1*n2*n3)))
  #LP
  third_day_price <-lp_solve("min",objective_function,third_day_non_negativity,third_day_relations,third_day_b)
  
  # output lista
  out <- list()
  
  out[["third_day_price"]] <- third_day_price # ebben minden eredmény benne van
  
  n_dual<-  nrow(third_day_non_negativity1)
  
  out[["payoffs"]] <- c(-third_day_onfin) # kifizetések
  out[["third_day_dual"]] <- third_day_price$dual[2:(n_dual+1)] # duál változók
  
  return(out)
}