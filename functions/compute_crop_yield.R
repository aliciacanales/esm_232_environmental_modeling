#' @title compute crop yield
#' @description
#' Function computes almond yield anomaly (ton/acre) given minimum temperature
#' (C) and precipitation (mm).
#' @author Alicia Canales and Zoe Zhou
#' @param climate_data climate data frame daily
#' @param t minimum temperature (C)
#' @param p total precipitation (mm)
#' @param  Tcoeff1 default=-0.015
#' @param  Tcoeff2 default=-0.0046
#' @param  Pcoeff1 default=-0.07
#' @param  Pcoeff2 default=0.0043
#' @return y max, min and mean yield anomaly for a input time series (ton/acre)
#' @examples compute_crop_yield(climate_data)
#' @references model derives from D.B. Lobell et al. Agricultural and Forest Meteorology 141 (2006) 208–218.

compute_crop_yield <- function(climate_data, Tcoeff1 = -0.015, Tcoeff2 = -0.0046, Pcoeff1 = -0.07, Pcoeff2 = 0.0043, intercept = 0.28){

  # extracting only relevant temperature data for the almond yield anomaly 
  t_feb <- climate_data %>%
    filter(month == '2') %>% # select relevant month 'February' for climate variable: t
    group_by(year) %>%
    summarise(temp_min = min(tmin_c)) # finding the minimum (lowest) temperature in February for a given year

  # extracting only relevant precipitation data for the almond yield anomaly 
  p_jan <- climate_data %>%
    filter(month == '1') %>% # select relevant month 'January' for climate variable: p
    group_by(year) %>%
    summarise(total_precipitation = sum(precip)) # finding the total precipitation in January for a given year

  
  # using the almond linear regression equation to compute yield anomaly 
  almond_yield = (Tcoeff1 * t_feb$temp_min) + (Tcoeff2 * t_feb$temp_min^2) + (Pcoeff1 * p_jan$total_precipitation) + (Pcoeff2 * p_jan$total_precipitation^2) + intercept

  # data frame that holds annual yield results
  yield <- data.frame(year = t_feb$year, 
                         yield_anomaly = almond_yield)

  # finding the min, max, and mean yield anomaly and adding it to 'yield' data frame
  yield %>% 
    mutate(minyield = min(yield$yield_anomaly),
           maxyield = max(yield$yield_anomaly),
           meanyield = mean(yield$yield_anomaly))

  return(yield)
}


