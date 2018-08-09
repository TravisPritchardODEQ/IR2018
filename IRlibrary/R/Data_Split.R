#' Data Split function
#' 
#' This function splits a data frame (df) into files of size n and saves csv to filepath
#' @param df data frame to be split
#' @param n row size to split dataframe into
#' @param filepath pathway to save file. Must end with /
#' @export
#' @examples
#'





Data_Split <- function(df, n, filepath) {
  
  num <- nrow(df)
  r  <- rep(1:ceiling(num/n),each=n)[1:num]
  d <- split(df,r)
  
  for(i in 1:length(d)){
    
    shortdf <- as.data.frame(d[i])
    names(shortdf) <- names(df)
    write.csv(shortdf, file=paste0(filepath,deparse(substitute(df)), "-", i, ".csv"))
    
  }
}


