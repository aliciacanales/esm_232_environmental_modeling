compute_forest_growth <- function(r, c, g, K, canopy = 50){
  
 # inputting if then statement to determine which carbon model to use
   if (c > canopy){
     
     # if carbon is above canopy threshold
    forest_growth = r * c
  } else {
    
    # if carbon is below canopy threshold
    forest_growth = g * (1 - c/K)
  }
  return(forest_growth)
}