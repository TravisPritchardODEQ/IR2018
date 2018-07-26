#' geo_mean
#' 
#' This function calculates a geometric mean for a vector
#' borrowed from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
#' Input is name of vector
#'@param x vector to calculate geometric mean
#'@export

geo_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  
}


