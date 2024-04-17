#..............Profit model..........................

compute_profit <- function(climate_data, labor = 50, cost = 15) {
  yield = compute_crop_yield(climate_data, Tcoeff1 = -0.015, Tcoeff2 = -0.0046, Pcoeff1 = -0.07, Pcoeff2 = 0.0043, intercept = 0.28)

  set.seed(123)  
  price = runif(nrow(yield), 1, 10) ## price we get for almond produced 
  
  income = price * yield$yield_anomaly
  total_cost = labor * cost
  net_yield = round(income - total_cost)
  
  profit = data.frame(year = yield$year, profit = net_yield)
  
  return(profit)
}

