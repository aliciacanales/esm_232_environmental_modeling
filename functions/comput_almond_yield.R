compute_crop_yield <- function(climate_data, t, p){
browser()
  t <- climate_data %>% 
    filter(month == '2') %>% 
    group_by(year) %>% 
    summarise(temp_min = which.min(tmin_c))
  
  p <- climate_data %>%  
    filter(month == '1') %>% 
    group_by(year) %>% 
    summarise(total_precipitation = sum(precip))

  
  y = (-0.015 * t$temp_min) - (0.0046 * t$temp_min) - (0.07 * p$total_precipitation) + (0.0043 * p$total_precipitation^2) + 0.28
  
  minyield = min(y)
  maxyield = max(y)
  meanyield = mean(y)

  return(list(minyield, maxyield, meanyield))
}

almond_yield <- compute_crop_yield(climate_data)

## the output =  maximum, minimum and mean yield anomoly for a input time series
## maximum almond yield anomaly should be approximately 1920 ton/acre
## min almond yield anomaly should be approximately -0.027 ton/acre
## mean almond yield anomaly should be approximately 182 ton/acre

