lp_solve <- function(exercise,objectiveFunction,constraint, relations,b){
# Megoldja az LP feladatot
  number_of_constraints <- dim(constraint)[1] # feltételek száma
  number_of_variables <- dim(constraint)[2] # változók száma
  
  # megoldja az LP feladatot
  LP <- make.lp(0,number_of_variables) # feladat deklarálása
  set.objfn(LP,objectiveFunction) # célfüggvény meghatározása
  lp.control(LP,sense=exercise) # min/max probléma
  set.type(LP, columns=number_of_variables, "real") # valós megoldás
  
  # korlátozó feltételek megadása
  for(i in 1:number_of_constraints){
    add.constraint(LP, constraint[i,],relations[i], b[i])
  }
  # előjelkötetlen változók deklarálása
  set.bounds(LP, lower = rep(-Inf, number_of_variables), upper = rep(Inf, number_of_variables), columns = 1: number_of_variables)
 
  
  
  ColNames <-paste0("theta",0,1:number_of_variables)
  RowNames <- paste0("c",1:number_of_constraints)
  dimnames(LP) <- list(RowNames, ColNames)
  
  # kimenti az LP-t
#    write.lp(LP,write_lp$name,type='lp')

  
   # LP megoldása
  solution <-solve(LP)
  # eredmények tárolása
  LP_solution <- list()
  # célfüggvény értéke
  LP_solution[["objective"]] <-get.objective(LP)
  # változók
  LP_solution[["variables"]] <- get.variables(LP)
  # duál változók, a kockázatsemleges árazáshoz kell
  LP_solution[["dual"]] <- get.dual.solution(LP)#[2:(number_of_constraints+1)]
  
  return(LP_solution)

}