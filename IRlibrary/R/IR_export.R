#' IR categories export function function
#' 
#' This function splits a data frame by column named OWRD_Basin 
#' @param df data frame to be split
#' @param location pathway to put exported files
#' @param parameter parameter of export file such as "DO_yearround_continuous"
#' @export
#' @examples
#'




IR_export <- function(df, location, parameter) {


# Get list of unique basins in dataset. Used for generating data for review
basins <- unique(df$OWRD_Basin) 


# Loop through data, and filter by OWRD basin, write csv file of all data in that basin
for(i in 1:length(basins)){
  
  Basin <- basins[i]
  print(paste("Writing table", i, "of",length(basins), "-", Basin ))
  
  by_basin <-  df %>%
    filter(OWRD_Basin == Basin)
  
  write.csv(by_basin, paste0(location, "/", parameter, "_IR_categories_",Basin,".csv"))
  
  
}

}