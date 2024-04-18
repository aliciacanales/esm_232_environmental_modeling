#..............Profit model..........................
#' @title compute profit
#' @description
#' Function computes profit from almond yield by calling compute_crop_yield function
#' @author Alicia Canales and Zoe Zhou
#' @param climate_data climate data frame daily
#' @param cost cost of labor in dollar, default = 15
#' @param labor number of people working for almond farm, default = 50
#' @param price of each almond yield sampled with random uniform as input
#' @param Tcoeff1 default=-0.015
#' @param Tcoeff2 default=-0.0046
#' @param Pcoeff1 default=-0.07
#' @param Pcoeff2 default=0.0043
#' @return data frame with estimated profit
#'
compute_profit <- function(climate_data, labor = 50, cost = 15) {

  # call almond yield function within profit function
  yield = compute_crop_yield(climate_data, Tcoeff1 = -0.015, Tcoeff2 = -0.0046, Pcoeff1 = -0.07, Pcoeff2 = 0.0043, intercept = 0.28)
  
  # general random uniform sample for price as input 
  price <- runif(n = 22, 100, 1000) ## market price for 1 ton of almonds for every year
  
  income = price * yield$yield_anomaly       # income equals to price per yield  times total almond yield
  total_cost = labor * cost                   # assume only cost is labor cost
  net_yield = round(income - total_cost)      #profit
  
  # error checking: make sure values are reasonable
  if (min(price) < 1)
    return(NA)
  
  # data frame that holds profit results
  profit = data.frame(year = yield$year, profit = net_yield)
  
  return(profit)
}

