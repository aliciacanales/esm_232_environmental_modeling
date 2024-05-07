compute_forest_growth <- function(time, c, params){
    
  forest_growth <- params$g * (1 - c/params$k)
    
  forest_growth <- ifelse(c < params$threshold, params$r * c, forest_growth)
   
  return(list(forest_growth))
}
