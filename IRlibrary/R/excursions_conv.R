#' excursions_conv
#' 
#' This function returns the number of excursions required to list as impaired for conventional pollutants,
#' @param n number of results
#' @export
#' @examples 
#' excursions_conv()


excursions_conv <- function(num){
  
 x = ifelse(num <= 18, 2, qbinom(0.90, num, 0.10, lower.tail = TRUE)+1 )
return(x)
  }


