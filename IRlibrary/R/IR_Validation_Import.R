#' IR_Validation_Import
#' 
#' This function rejoins the data that was removed from the process during the data validation step. 
#' IR_Validation creates a table that must be reviewed. If data labled as invalid is actually valid,
#' change the Conclusion field to read "Valid"
#' @param file File that contains reviewd data validation table
#' @param df Dataframe that results from IR_Validation function. For the IR, this will be the results of the datapull script. 
#' @export

IR_Validation_Import <- function(file, df){

library(tidyverse)
library(lubridate)

validated <- read.csv(file, stringsAsFactors = FALSE) 

data_to_merge <- validated %>%
  filter(validation == "Valid" | Conclusion %in% c("Valid", "valid")) %>%
  mutate(SampleStartDate = as.Date(parse_date_time(SampleStartDate, c("mdy", "ymd")))) %>%
  select(-Au_valid_count, -AU_total_count, 
         -perc_valid, -per99, -per1, 
         -AmbDatarange, -StdRef)


  complete_data <- df %>%
  bind_rows(data_to_merge)

return(complete_data)
}
