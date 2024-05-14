
#'  Simple population growth
#' @param time time
#' @param P initial population
#' @param r intrinsic growth rate 
#' @return derivative of population with time 
#' @examples use with ode solver
#' ode(y=1,time=c(1;100),dexppop, parms=c(0.012))

dexppop = function(time, P, r, K = 75) {
  
  dexpop = r*P
  if (P > K){
    dexpop = 0
  }
  
  return(list(dexpop))
}
