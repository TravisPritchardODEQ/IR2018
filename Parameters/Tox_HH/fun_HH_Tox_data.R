require(rgdal)
require(RODBC)
require(tidyverse)
require(IRlibrary)

HH_tox_data <- function(database) {
  
  

  print("Fetch HH Tox data from IR database")
# connect to IR database view as a general user
# import bacteria data
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_ToxHH") 
  
  
  odbcClose(IR.sql)
#   
  #######################################################
  ###         Temporary save of Results import -      ###
  ###         Remove this when more data is in db     ###
  #######################################################
  
  #save(Results_import, file = "Parameters/Tox_HH/Results.RData")
  
  # #Load in saved dataset
  # load("Parameters/Tox_HH/Results.RData")
  # 
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  
  
  ############ - I did not adjust anything below here. 
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  # Get all the standards to be used when dealing with the censored data
  Results_crit <- Results_import %>%
    # Get lowest criteria value to set censored results
    mutate(lowest_crit = pmin(WaterOrganism, Organism, Organism_SW, na.rm = TRUE))
  
  
  print("Modify censored data")
  
  #run the censored data function to set censored data. This will use the lowest crit value from above
  Results_censored <- Censored_data(Results_crit, crit = `lowest_crit` ) %>%
    mutate(Result_cen = as.numeric(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored %>%
    filter(!is.na(Result_cen))
  
  print("Data fetch and censored data modifications complete")

return(Results_censored)
  
}