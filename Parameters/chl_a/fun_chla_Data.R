require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)




chla_data <- function(database) {
  print("Fetch Chl data from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_Chl") 
  
  
  odbcClose(IR.sql)
  
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  
  # Data validation ---------------------------------------------------------
  
  print("NO data validation")
  
  # # Load validation table
  # load("Validation/anom_crit.Rdata")
  # 
  # Results_valid <- IR_Validation(Results_import, anom_crit, "Chl")
  
  
  
  # Censored data ------------------------------------------------------------
  
  load("Other tools/chl_aggregate_data.Rdata")
  
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
  
  
  print("Modify censored data")
  
  #run the censored data function to set censored data. This will use the lowest crit value from above 
  Results_censored <- Censored_data(results_to_validate, crit = `Chla_Criteria` ) %>%
    mutate(Result_cen = as.numeric(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored%>%
    filter(!is.na(Result_cen))
  
  print("Data fetch and censored data modifications complete")
  
  
  return(Results_censored)
}