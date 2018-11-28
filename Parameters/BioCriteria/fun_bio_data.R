Bio_data <- function(database) {
  
  
  require(tidyverse)
  require(RODBC)
  require(IRlibrary)  
  print("Fetch bio data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_bio <-    sqlFetch(IR.sql, "dbo.BioCriteria") 
  
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_bio), "results from", length(unique(Results_bio$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_bio %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_bio }

  
  
