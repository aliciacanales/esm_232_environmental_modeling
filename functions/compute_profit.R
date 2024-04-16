#..............Profit model..........................

compute_profit <- function(yield, irr, cost = 5, price){
  browser()
  
  yield = compute_crop_yield(climate_data, Tcoeff1 = -0.015, Tcoeff2 = -0.0046, Pcoeff1 = -0.07, Pcoeff2 = 0.0043, intercept = 0.28)
  
  irr = runif(nrow(yield, 1, 10)) 
  price = runif(nrow(yield, 1, 10))
  
  income = price * yield
  cost = irr * cost
  net_yield = income - cost
  
  return(net_yield)
}
