
require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)
library(openxlsx)




DO_data <- function(database) {
  print("Fetch DO data from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_DO") 
  
  
  odbcClose(IR.sql)
  
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations in",
              length(unique(Results_import$AU_ID)), "AUs"))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  

# Data aggregation --------------------------------------------------------

 

  Results_import <- Results_import %>%
    filter(is.na(Statistical_Base) | Statistical_Base != 'Delta')

  load("Other tools/aggregate_data.Rdata")
  
  data_to_agg <- Results_import %>%
    left_join(aggregate_data, by = 'Result_UID') %>%
    filter(!is.na(group)) %>%
    arrange(group) %>%
    group_by(group) %>%
    mutate(analysis_comment = paste0(Result_UID, collapse = ", "),
           keep = ifelse(row_number() == 1, 1, 0 )) %>%
    mutate(analysis_comment = paste("Result is the average of result_UIDs:",analysis_comment, " - due to multiple results at same date")) %>%
    ungroup() %>%
    select(Result_UID, mean_result, keep, analysis_comment)
  
  results_to_validate <- Results_import %>%
    left_join(data_to_agg) %>%
    filter(keep == 1 | is.na(keep)) %>%
    mutate(IRResultNWQSunit = ifelse(!is.na(mean_result), mean_result, IRResultNWQSunit )) %>%
    select(-mean_result, -keep)
  # Data validation ---------------------------------------------------------  
  
  print("Validating Data")

  # Load validation table
  load("Validation/anom_crit.Rdata")


  Results_valid <- results_to_validate
  
  

  return(Results_valid)
  
}