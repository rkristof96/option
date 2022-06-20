parameter_sensitivity <- function(){
# a lognormális eloszlás várható értékének és szórásának függvényében vizsgálom az opció árát
# csak 1 időszakra, hogy lefusson és értelmes legyen az eredmény
repetition <- 25
# minden paeraméterre 25-ször árazom az opciót és átlagolok, hogy a véletlen hatást csökkentsem
  
mu <- c(1.25,1.5,1.75,2,2.25, 2.5,2.75)
sigma <- c(0.5,0.75,1,1.5,2,2.5,3)

mu_price <- matrix(NA, nrow=repetition, ncol = length(mu))
sigma_price <- matrix(NA, nrow=repetition, ncol = length(sigma))

for(i in 1:length(mu)){
  for(j in 1:repetition){
    
  #threedays <- read_input(mu[i],1,15,17,20)
  threedays <- read_input(mu[i],1,200,17,20)
  step1_threedays <-  first_day_pricing(threedays,FALSE)
  #step2_threedays <- second_day_pricing(step1_threedays,threedays)
  #step3_threedays <- third_day_pricing(step1_threedays,step2_threedays,threedays)
  
  # mu_price[j,i] <-step3_threedays$third_day_price$objective # derivatíva ára
  mu_price[j,i] <-step1_threedays$first_day_price$objective # derivatíva ára

  }}

for(i in 1:length(sigma)){
  for(j in 1:repetition){
    #threedays <- read_input(mu[i],1,15,17,20,TRUE,TRUE)
    threedays <- read_input(2,sigma[i],200,17,20)
    step1_threedays <-  first_day_pricing(threedays,FALSE)
    #step2_threedays <- second_day_pricing(step1_threedays,threedays)
    #step3_threedays <- third_day_pricing(step1_threedays,step2_threedays,threedays)
    
    # mu_price[j,i] <-step3_threedays$third_day_price$objective # derivatíva ára
    sigma_price[j,i] <-step1_threedays$first_day_price$objective # derivatíva ára
    
  }}


mu_mean <- apply(mu_price,2,FUN = mean)
sigma_mean <- apply(sigma_price,2,FUN = mean)

data_mu <- data.frame(name= mu, value=round(mu_mean,5))
data_sigma <- data.frame(name= sigma, value=round(sigma_mean,5))



muplot <- ggplot(data=data_mu, aes(x=as.factor(name),y=value)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.75, position = position_dodge(width=0))+
  geom_text(aes(label=value), vjust=-0.3, size=4) +
  ggtitle("The average price of the option in the case of one period, "~mu~"=1.25,1.5,1.75,2,2.25, 2.5,2.75 and "~sigma~"=1")+
  theme_minimal()+
  xlab(expression(~mu)) + ylab("Price")+theme( plot.title = element_text(size = 20, face = "bold"), axis.text=element_text(size=17), axis.text.x = element_text(size=17), axis.text.y = element_text(size=17))

sigmaplot <- ggplot(data=data_sigma, aes(x=as.factor(name),y=value)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.75, position = position_dodge(width=0))+
  geom_text(aes(label=value), vjust=-0.3, size=4) +
  ggtitle("The average price of the option in the case of one period, "~mu~"= 2 and "~sigma~"=0.5,0.75,1,1.5,2,2.5,3")+
  theme_minimal()+
  xlab(expression(~sigma)) + ylab("Price")+theme( plot.title = element_text(size = 20, face = "bold"), axis.text=element_text(size=17), axis.text.x = element_text(size=17), axis.text.y = element_text(size=17))



out <- list()
out[["mu"]] <- muplot
out[["sigma"]] <- sigmaplot
return(out)
}
