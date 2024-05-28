#' Compute Forest Growth
#'
#' @param time The time sequence the function will run over
#' @param c The size of the forest in units of carbon
#' @param params List of parameters that will calculate forest growth. Should include k, r, g, and threshold
#'
#' @return The forest growth using either exponential rate of change or linear rate of change 
#' 
compute_forest_growth <- function(time, c, params){
    
  forest_growth <- params$g * (1 - c/params$k)
    
  forest_growth <- ifelse(c < params$threshold, params$r * c, forest_growth)
   
  return(list(forest_growth))
}
