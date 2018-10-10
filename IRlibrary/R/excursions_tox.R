#' excursions_tox
#' 
#' This function returns the number of excursions required to list as impaired for toxic pollutants,
#' @param n number of results
#' @export
#' @examples 
#' excursions_tox()


excursions_tox <- function(n){
  
  x = ifelse(n <= 18, 2, qbinom(0.90, n, 0.05, lower.tail = TRUE)+1)
  return(x)
}


