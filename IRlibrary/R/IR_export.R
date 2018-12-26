#' IR categories export function function
#' 
#' This function splits a data frame by column named OWRD_Basin 
#' @param df data frame to be split
#' @param location pathway to put exported files
#' @param parameter parameter of export file such as "DO_yearround_continuous"
#' @param type Either catergories or data. Use din filename to distinguish between categories or data
#' @export
#' @examples
#'




IR_export <- function(df, location, parameter, type) {


# Get list of unique basins in dataset. Used for generating data for review
basins <- unique(df$OWRD_Basin) 

write.csv(df, paste0(location, "/", parameter, "_IR_",type,"_ALLDATA.csv"))

# Loop through data, and filter by OWRD basin, write csv file of all data in that basin
for(i in 1:length(basins)){
  
  Basin <- basins[i]
  print(paste("Writing table", i, "of",length(basins), "-", Basin ))
  
  by_basin <-  df %>%
    filter(OWRD_Basin == Basin)
  
  write.csv(by_basin, paste0(location, "/", parameter, "_IR_",type,"_",Basin,".csv"))
  
  
}

}