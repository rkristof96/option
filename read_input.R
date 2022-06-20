read_input <- function(lmu, lsigma,n1,n2,n3){
  # ez a fggv?ny az el?re megadott adatokat t?rolja egy list?ban
  input <- list()
  
  input[["mu"]] <- lmu # lognorm?lis eloszl?s v?rhat? ?rt?ke
  input[["sigma"]] <- lsigma # lognorm?lis eloszl?s sz?r?sa
  input[["number_of_assets"]] <-3 # h?ny db r?szv?ny van # ez v?gig 3+1 k?szp?nz vagy 0%-os k?tv?ny
  #n1 - els? id?szakban h?ny fel? ?gazik az ?rfolyam
  #n2 - m?sodik id?szakban h?ny fel? ?gazik az ?rfolyam
  #n3 - harmadik id?szakban h?ny fel? ?gazik az ?rfolyam
  input[["n1"]] <- n1 
  input[["n2"]] <- n2
  input[["n3"]] <- n3
  
  # cash_value <- 100 # kezdeti ?rt?k / p?nz ?rt?ke
  input[["cash_value"]] <- 100
  # kezdeteben min a 3 r?szv?ny 100-at ?r
  starting_price <-rep(100,input$number_of_assets) # részvények vektorát gyártom le
  
  input[["starting_price"]] <- starting_price

  
  return(input)
}
