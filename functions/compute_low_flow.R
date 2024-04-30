#' computing combined performance metrics in lowflow during summer months 
#' @param m model estimates
#' @param o observations
#' @param month month
#' @param day day
#' @param year year
#' @param low_flow_months which to use default (June to August 6:8)
#' @param wts (vector of 4 for annual_min_err, annual_min_corr, low_month_cor, low_month_err)
#' @param max_err_annual_min
#' @param max_err_low_month
#' @return annual_min_err, annual_min_corr, low_month_cor, low_month_err, combined

compute_low_flow <- function(m, o, month, day, year, low_flow_months = c(6:8), 
                             max_err_annual_min=NULL, max_err_low_month=NULL, 
                             wts=c(0.1,0.1,0.4,0.4)) {
  # prepare data frame
  flw_data = cbind.data.frame(m, o, month, day, year) 
  
  # Yearly metrics
  # clean data frame to include min for each year in model and observed data
  flw_data_clean <- flw_data %>% 
    group_by(year) %>%
    summarize(observed_low_flow = min(o), model_low_flow = min(m))
  # calculate metrics for annual min
  annual_min_err = mean(flw_data_clean$model_low_flow - flw_data_clean$observed_low_flow)
  annual_min_cor = cor(flw_data_clean$model_low_flow, flw_data_clean$observed_low_flow)
  # if user doesn't specify maximum errors use 50% of mean observed values
  if (is.null(max_err_annual_min))
  { max_err_annual_min = 0.5*mean(flw_data_clean$observed_low_flow)}
  
  # monthly metrics
  # clean data frame for monthly data
  flw_data_tmp = flw_data %>% 
    group_by(month, year) %>% 
    summarize(model=sum(m), obs=sum(o))
  # extract low flow months
  low = subset(flw_data_tmp, month %in% c(6:8))
  low_month_err = mean(low$model - low$obs)
  low_month_cor=cor(low$model, low$obs)
  # if user doesn't specify maximum errors use 50% of mean observed values
  if (is.null(max_err_low_month))
  { max_err_low_month = 0.5*mean(low$obs)}
  
  # transform error metrics to have the same scale as correlation  
  annual_min_err_trans = max(0, (1-abs(annual_min_err/max_err_annual_min)))
  low_month_err_trans = max(0, (1-abs(low_month_err/max_err_low_month)))
  
  # apply heavier weight on monthly metrics
  wts = wts/sum(wts) # normalize to sum to 1 
  # calculate combined metric score
  combined = wts[1]*annual_min_err_trans + wts[2]*annual_min_cor+
    wts[3]*low_month_cor + wts[4]*low_month_err_trans
  
  return(list(annual_min_err = annual_min_err, annual_min_cor = annual_min_cor,
              low_month_err = low_month_err, low_month_cor = low_month_cor,
              combined=combined))
}

test <- compute_low_flow(m = sager$model, o = sager$obs, month = sager$month ,day = sager$day, year = sager$year, low_flow_months = c(6:8))

