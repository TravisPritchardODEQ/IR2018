
require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)
library(openxlsx)




Temp_data_narrative <- function(database) {
  print("Fetch Temperature data from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_Temperature_Narrative") 
  
  
  odbcClose(IR.sql)
  
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  
  # Data validation ---------------------------------------------------------
  
  # print("Validating Data")
  # 
  # # Load validation table
  # load("Validation/anom_crit.Rdata")
  # 
  # Results_valid <- IR_Validation(Results_import, anom_crit, "Temperature")
  # 
  Results_valid <- Results_import
  
  
  # Censored data ------------------------------------------------------------
  
  
  
  print("Modify censored data")
  
  #run the censored data function to set censored data. This will use the lowest crit value from above 
  Results_censored <- Censored_data(Results_valid, crit = `Temp_C` ) %>%
    mutate(Result_cen = as.numeric(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored%>%
    filter(!is.na(Result_cen))
  
  print("Data fetch and censored data modifications complete")
  
  return(Results_censored)
}