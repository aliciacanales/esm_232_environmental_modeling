#' Computing Energy Produced from a PV system
#'
#' @param A solar panel area in m2
#' @param H annual average solar radiation in kWh
#' @param r panel yield (0 - 1)
#' @param PR performance ratio (0 - 1)
#'
#' @return energy produced by a PV system

energy_produced <- function(A, H, r = 0.2, PR = 0.75){
  
  # calculate energy (kWh) using inputs A (area) and H (annual average solar radiation)
  energy = A * r * H * PR
  
  return(energy)
}

