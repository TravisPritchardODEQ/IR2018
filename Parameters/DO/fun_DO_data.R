
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
  
  
  # Data validation ---------------------------------------------------------

  Results_import <- Results_import %>%
    filter(is.na(Statistical_Base) | Statistical_Base != 'Delta')
  
  
  print("Validating Data")

  # Load validation table
  load("Validation/anom_crit.Rdata")

  Results_valid <- IR_Validation(Results_import, anom_crit, "DO")

  return(Results_valid)
  
}