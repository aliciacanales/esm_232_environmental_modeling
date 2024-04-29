## computing percent change in lowflow during summer months 

compute_lowflow <- function(m ,o, month, day, year, low_flow_months = c(6:8) ) {
  browser()
  
  flow_data = cbind.data.frame(m,o, month, day, year) %>% 
    group_by(year) %>%
    summarize(observed_low_flow = min(o), model_low_flow = min(m))
 
  annual_min_err = mean(flow_data$model_low_flow-flow_data$observed_low_flow)
  annual_min_cor = cor(flow_data$model_low_flow, flow_data$observed_low_flow)
  
  tmp = flow %>% 
    group_by(month, year) %>% 
    summarize(model=sum(m), obs=sum(o))
  
  low = subset(tmp, month %in% c(6:8))
  low_month_err = mean(low$model-low$obs)
  low_month_cor=cor(low$model, low$obs)
  
  return(list(flow, flow_per_change_m, flow_per_change_o))
}

test <- compute_lowflow(m = sager$model, o = sager$obs, month = sager$month ,day = sager$day, year = sager$year, low_flow_months = c(6:8))

