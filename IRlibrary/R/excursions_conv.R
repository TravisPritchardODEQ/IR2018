#' excursions_conv
#' 
#' This function returns the number of excursions required to list as impaired for conventional pollutants,
#' @param n number of results
#' @export
#' @examples 
#' excursions_conv()


excursions_conv <- function(n){
  
 x = ifelse(n <= 18, 2, qbinom(0.90, n, 0.10, lower.tail = TRUE)+1 )
return(x)
  }


