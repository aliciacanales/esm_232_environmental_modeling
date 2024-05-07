compute_forest_growth <- function(time, r, c, g, K, threshold = 50){
  
  forest_growth <- r * c
  
  if (c < threshold){
    forest_growth <- g * (1 - c/K)
  }
  
   return(list(forest_growth))
}