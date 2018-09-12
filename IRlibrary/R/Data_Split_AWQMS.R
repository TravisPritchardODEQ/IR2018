#' Data Split function
#' 
#' This function splits a data frame (df) into files of size n and saves csv to filepath
#' @param df data frame to be split
#' @param split_on name of the column in dataframe to be split on
#' @param size row length to trigger splitting. Files will be longer than this length
#' @param filepath string for pathway to save file. Must end with /
#' @export
#' @examples
#'


)

Data_Split_AWQMS <- function(df, split_on, size, filepath) {
  
  require(openxlsx)
  tempsplit <- split(df, df[[split_on]])
  length(tempsplit)
  datasplit <- list()
  
  j = 1
  for (i in 1:length(tempsplit)) {
    datasplit[[i]] <- bind_rows(tempsplit[[i]])
    
    if (nrow(bind_rows(datasplit)) > size | i == length(tempsplit)) {
      write.xlsx(bind_rows(datasplit),
                file = paste0(filepath, deparse(substitute(df)), "-", j, ".xlsx"), na = "")
      datasplit <- NULL
      j <- j + 1
    }
  }
}