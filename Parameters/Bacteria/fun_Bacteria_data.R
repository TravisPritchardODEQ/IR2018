

Bacteria_data <- function(database) {
  
  
require(tidyverse)
require(RODBC)
require(IRlibrary)  
  print("Fetch bacteria data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_Bacteria") 
  
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  

# Data Validation ---------------------------------------------------------

  
  print("Validating Data")
  
  # Load validation table
  load("Validation/anom_crit.Rdata")
  
  Results_valid <- IR_Validation(Results_import, anom_crit, "Bacteria")
  
  rm(anom_crit)
  
  return(Results_valid)
  
}
