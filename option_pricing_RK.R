##########################
#### Opcióárazás HF ######
##########################
library(lpSolve)
library(lpSolveAPI)
library(nonparaeff)

source("share_price.R")

# rögzítsük a véletlenszámgenerátor kezdõértékét
set.seed(1000)
# 3 termékkel dolgozunk
# kezdetben minden 100-ba kerül

number_of_assets <- 3
number_of_scenarios <- 2

cash_value <- 100

starting_price <-rep(100,number_of_assets) # részvények vektorát gyártom le

#______________________________________________________________________________________________________________

first_day <- share_price(starting_price, number_of_scenarios,1) # legenerálja az új részvényárakat és vektorként visszadja

first_day_cash <- cbind(rep(cash_value,number_of_scenarios),matrix(first_day,ncol=number_of_assets, nrow=number_of_scenarios, byrow=T))
first_day_onfin <-apply(cbind(first_day_cash[,3]-first_day_cash[,2],0),1,FUN=max)
first_day_non_negativity <-rep(0,number_of_scenarios)
first_day_b<-c(-first_day_onfin,first_day_non_negativity)

first_day_cash_column <- as.vector(t(first_day_cash))

first_day_relations <-rep(c("=",">="),c(number_of_scenarios,number_of_scenarios))


first_day_non_negativity <- matrix(rep(first_day_cash_column,number_of_scenarios), ncol = number_of_scenarios)

aux_first_day_non_negativity <-matrix(NA,nrow=number_of_scenarios, ncol = (number_of_assets+1))


for(i in 1:number_of_scenarios){
  first_index<-(1+(i-1)*(number_of_assets+1))
  last_index <- i*(number_of_assets+1)
  
  first_day_non_negativity[-(first_index:last_index),i]<-0
  aux_first_day_non_negativity[i,]<-first_day_non_negativity[(first_index:last_index),i]
}

# önfinanszírozási korlát
first_day_non_negativity_part1 <- cbind(-aux_first_day_non_negativity,t(first_day_non_negativity))
# nem negativitási korlát
first_day_non_negativity_part2 <- cbind(matrix(0,ncol=(number_of_assets+1),nrow=number_of_scenarios),t(first_day_non_negativity))

first_day_non_negativity <-rbind(first_day_non_negativity_part1, first_day_non_negativity_part2)


objective_function <- c(rep(1,4),rep(0,4*number_of_scenarios))

# ez nem jó! Nem tud elõjelkötetlen változóval dolgozni!
#first_day_price <- lpSolve::lp("min",objective_function,first_day_non_negativity,first_day_relations,first_day_b)
first_day_free_variables <- rep(0,4*number_of_scenarios+4) # nem negatív változók
#first_day_free_variables <- rep(1,4*number_of_scenarios+4) #elõjelkötetlen változók



# normalization<- which(first_day_b<0)[1]
# if(length(normalization)!=0){
#   first_day_b <- -(first_day_b/first_day_b[normalization])
# }

# first_day_price <- nonparaeff::lp2("min",objective_function,first_day_non_negativity,first_day_relations,first_day_b, free.var = first_day_free_variables)
first_day_price <-lp2("min",objective_function,first_day_non_negativity,first_day_relations,first_day_b, free.var = first_day_free_variables)

first_day_price$objval

first_day_price$solution.free

first_day_price$solution
first_day_price$status
first_day_price$presolve

##_______________________________________________________________________________________________________________
second_day <- share_price(first_day, number_of_scenarios,2) # legenerálja az új részvényárakat és vektorként visszadja

second_day_cash <- cbind(rep(cash_value,number_of_scenarios),matrix(second_day,ncol=number_of_assets, nrow=number_of_scenarios^2, byrow=T))
second_day_onfin <-apply(cbind(second_day_cash[,3]-second_day_cash[,2],0),1,FUN=max)
second_day_non_negativity <-rep(0,number_of_scenarios^2)
second_day_b <- c(-first_day_onfin,-second_day_onfin,second_day_non_negativity)

second_day_cash_column <- as.vector(t(second_day_cash))

second_day_relations <-rep(c("=",">="),c((sum(number_of_scenarios^(0:2))-1),number_of_scenarios^2))

second_day_non_negativity <- matrix(rep(second_day_cash_column,number_of_scenarios^2), ncol = number_of_scenarios^2)

aux_second_day_non_negativity <-matrix(0,nrow=number_of_scenarios^2, ncol = (number_of_assets+1)*number_of_scenarios)


for(i in 1:number_of_scenarios^2){
  second_index<-(1+(i-1)*(number_of_assets+1))
  last_index <- i*(number_of_assets+1)
  
  second_day_non_negativity[-(second_index:last_index),i]<-0
  
  second_index_aux<-(1+((i-1)%/%number_of_scenarios)*(number_of_assets+1))
  last_index_aux <- (((i-1)%/%number_of_scenarios+1)*(number_of_assets+1))
  
  aux_second_day_non_negativity[i,second_index_aux:last_index_aux]<-second_day_non_negativity[(second_index:last_index),i]
}

first_day_non_negativity_part1_concatenatedzeros <- cbind(first_day_non_negativity_part1,matrix(0,nrow=number_of_scenarios, ncol = number_of_scenarios^2*(number_of_assets+1)))


# önfinanszírozási korlát
second_day_non_negativity_part1 <- cbind(-aux_second_day_non_negativity,t(second_day_non_negativity))
# nem negativitási korlát
second_day_non_negativity_part2 <- cbind(matrix(0,ncol=(number_of_assets+1)*(number_of_scenarios),nrow=number_of_scenarios^2),t(second_day_non_negativity))

second_day_non_negativity <-rbind(second_day_non_negativity_part1, second_day_non_negativity_part2)

second_day_non_negativity <-cbind(matrix(0,ncol=number_of_assets+1, nrow = number_of_scenarios^2*2),second_day_non_negativity)

second_day_non_negativity <- rbind(first_day_non_negativity_part1_concatenatedzeros,second_day_non_negativity)

objective_function <- c(rep(1,4),rep(0,4*(sum(number_of_scenarios^(0:2))-1)))

second_day_free_variables <- rep(0,4*sum(number_of_scenarios^(0:2))) # nem negatív változók

second_day_price <-lp2("min",objective_function,second_day_non_negativity,second_day_relations,second_day_b, free.var = second_day_free_variables)

second_day_price$objval

second_day_price$solution.free
second_day_price$solution

#################################################################################
third_day <- share_price(first_day, number_of_scenarios,3) # legenerálja az új részvényárakat és vektorként visszadja

third_day_cash <- cbind(rep(cash_value,number_of_scenarios),matrix(third_day,ncol=number_of_assets, nrow=number_of_scenarios^3, byrow=T))
third_day_onfin <-apply(cbind(third_day_cash[,3]-third_day_cash[,2],0),1,FUN=max)
third_day_non_negativity <-rep(0,number_of_scenarios^3)
third_day_b<-c(-first_day_onfin,-second_day_onfin,-third_day_onfin,third_day_non_negativity)

third_day_cash_column <- as.vector(t(third_day_cash))

third_day_relations <-rep(c("=",">="),c((sum(number_of_scenarios^(0:3))-1),number_of_scenarios^3))

third_day_non_negativity <- matrix(rep(third_day_cash_column,number_of_scenarios^3), ncol = number_of_scenarios^3)

aux_third_day_non_negativity <-matrix(0,nrow=number_of_scenarios^3, ncol = (number_of_assets+1)*number_of_scenarios^2)


for(i in 1:number_of_scenarios^3){
  third_index<-(1+(i-1)*(number_of_assets+1))
  last_index <- i*(number_of_assets+1)
  
  third_day_non_negativity[-(third_index:last_index),i]<-0
  
  third_index_aux<-(1+((i-1)%/%number_of_scenarios)*(number_of_assets+1))
  last_index_aux <- (((i-1)%/%number_of_scenarios+1)*(number_of_assets+1))
  
  aux_third_day_non_negativity[i,third_index_aux:last_index_aux]<-third_day_non_negativity[(third_index:last_index),i]
}

first_day_non_negativity_part1_concatenatedzeros <- cbind(first_day_non_negativity_part1,matrix(0,nrow=number_of_scenarios, ncol = (number_of_scenarios^3+number_of_scenarios^2)*(number_of_assets+1)))
second_day_non_negativity_part1_concatenatedzeros <- cbind(matrix(0,nrow=number_of_scenarios^2, ncol = (1+number_of_scenarios)),second_day_non_negativity_part1, matrix(0,nrow=number_of_scenarios^2, ncol =number_of_scenarios^3*(number_of_assets+1)))

# önfinanszírozási korlát
third_day_non_negativity_part1 <- cbind(matrix(0,nrow = number_of_scenarios^3,ncol = (1+number_of_scenarios)*(number_of_assets+1)),-aux_third_day_non_negativity,t(third_day_non_negativity))
# nem negativitási korlát
third_day_non_negativity_part2 <- cbind(matrix(0,ncol=(number_of_assets+1)*(sum(number_of_scenarios^(0:2))),nrow=number_of_scenarios^3),t(third_day_non_negativity))

third_day_non_negativity <-rbind(third_day_non_negativity_part1, third_day_non_negativity_part2)

# third_day_non_negativity<-cbind(matrix(0,ncol=number_of_assets+1, nrow = number_of_scenarios^2*2),third_day_non_negativity)

third_day_non_negativity <- rbind(first_day_non_negativity_part1_concatenatedzeros,second_day_non_negativity_part1_concatenatedzeros,third_day_non_negativity)

objective_function <- c(rep(1,4),rep(0,4*(sum(number_of_scenarios^(0:3))-1)))

third_day_free_variables <- rep(0,4*sum(number_of_scenarios^(0:3))) # nem negatív változók

third_day_price <-lp2("min",objective_function,third_day_non_negativity,third_day_relations,third_day_b, free.var = third_day_free_variables)

third_day_price$objval

third_day_price$solution.free
third_day_price$solution

###############################################################################

##_______________________________________________________________________________________________________________












paste0("theta",day,"_",1:number_of_scenarios)




