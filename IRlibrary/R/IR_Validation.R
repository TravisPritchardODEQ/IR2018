#' IR_Validation
#' 
#' This function performs the IR data validation process. Results are compared to oregon ambient
#' data for 1980 - 2017. Data outside the 99th percentile are labeled invalid. A table of Invalid data
#' is written to Parameters/Invalid_data. Assessment Units with no invalid data are returned as dataframe.
#' Input is the imported results dataframe, the anom_crit dataframe describing the oregon ambient data,
#' and the parameter classification e.g. Bacteria, pH. etc.
#' @param Results_import dataframe for the imported results
#' @param anom_crit dataframe for the ambient program criteria
#' @param parameter parameter classification used for naming invalid data file
#' @export 
 


IR_Validation <- function(Results_import, anom_crit, parameter) {
 
  require(tidyverse)
  require(openxlsx)
   
  Res_validation <- Results_import %>%
    left_join(anom_crit, by = c("Pollutant_DEQ WQS" = "char")) %>%
    mutate(validation = ifelse(Result4IR < per99 |Result4IR > per1 | is.na(per99) , "Valid", "Invalid")) %>%
    group_by(AU_ID) %>%
    mutate(Au_valid_count = sum(validation == "Valid"),
           AU_total_count = n() ) %>%
    ungroup() %>%
    mutate(perc_valid = Au_valid_count/AU_total_count)
   
   invalid_data <- Res_validation %>%
     filter(validation == "Invalid") 
   
   
   if (nrow(invalid_data) == 0) {
     print("No Invalid Data")
   }
   
   write.xlsx(invalid_data, paste0("Parameters/Invalid_data/", parameter, ".xlsx"))
   
   Valid_AUs <- Res_validation %>%
     filter(perc_valid == 1) 
  
  return(Valid_AUs)
}
