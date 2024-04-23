## computing percent change in lowflow during summer months 

compute_lowflow <- function(m ,o, month, day, year, low_flow_months = c(6:8) ) {
  browser()
  flow = cbind.data.frame(m,o, month, day, year)
  
  low_flow = flow %>% 
    group_by(year) %>% 
    summarize(observed_low_flow = min(o), model_low_flow = min(m))
  
  low_flow$flow_per_change_o = for (i in low_flow$observed_low_flow) {
    ((-i - i) / -i) * 100
      }

  low_flow$flow_per_change_m =
    for (i in low_flow$model_low_flow) {
      ((-i - i) / -i) * 100
        }

  summer_flow = low_flow %>% 
    group_by(year, month) %>% 
    summarize(flow_per_change_o = mean(flow_per_change_o),
              flow_per_change_m = mean(flow_per_change_o))
  
  flow = subset(summer_flow, month %in% low_flow_months)
  
  flow_per_change_o = summer_flow$flow_per_change_o
  flow_per_change_m = summer_flow$flow_per_change_m
  
  return(list(flow, flow_per_change_m, flow_per_change_o))
}

test <- compute_lowflow(m = sager$model, o = sager$obs, month = sager$month ,day = sager$day, year = sager$year, low_flow_months = c(6:8))

