#' DO saturation percent calculator
#' 
#' This function will calculate DO saturation percentage 
#' based on DO mg/L values, temperature in C, and elevcation in ft
#' This function is based on the equation found in The Dissolved
#' Ocygen Water Quality Standard Implementatiion Guidence.
#' This function differs from the oxySol function in the wql package
#' because it calcultaes the percentage dirrectly and incorporates elevation,
#' as opposed to pressure
#' 
#' @param DO DO value in mg/L
#' @param TempC Temperature value in C
#' @param elevft Elevation value in feet
#' @export
#' @examples 
#' DOSat.calc()

DOSat_calc <- function(DO, TempC, elevft) {
  DO / (exp(-139.34411 + ((1.575701*10^5)/(Temp+273.15)) - 
              ((6.642308 * 10^7)/((Temp+273.15)^2)) +  
              ((1.243800 * 10^10)/((Temp+273.15)^3)) -
              ((8.621949 * 10^11)/((Temp+273.15)^4))) * 
          (1 - (0.0001148 * elev/3.281 ))) * 100
}
