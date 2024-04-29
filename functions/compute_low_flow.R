## computing percent change in lowflow during summer months 

compute_low_flow <- function(m, o, month, day, year, low_flow_months = c(6:8) ) {
  
  flw_data = cbind.data.frame(m, o, month, day, year) 
  
  flw_data_clean <- flw_data %>% 
    group_by(year) %>%
    summarize(observed_low_flow = min(o), model_low_flow = min(m))
 
  annual_min_err = mean(flw_data_clean$model_low_flow - flw_data_clean$observed_low_flow)
  annual_min_cor = cor(flw_data_clean$model_low_flow, flw_data_clean$observed_low_flow)
  
  flw_data_tmp = flw_data %>% 
    group_by(month, year) %>% 
    summarize(model=sum(m), obs=sum(o))
  
  low = subset(flw_data_tmp, month %in% c(6:8))
  low_month_err = mean(low$model - low$obs)
  low_month_cor=cor(low$model, low$obs)
  
  return(list(annual_min_err = annual_min_err, annual_min_cor = annual_min_cor,low_month_err = low_month_err, low_month_cor = low_month_cor))
}

test <- compute_lowflow(m = sager$model, o = sager$obs, month = sager$month ,day = sager$day, year = sager$year, low_flow_months = c(6:8))

