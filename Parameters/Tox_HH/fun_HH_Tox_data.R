require(rgdal)
require(RODBC)
require(tidyverse)

HH_tox_data <- function(database) {
  
  

  print("Fetch HH Tox data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
#   IR.sql <-  odbcConnectAccess2007("//deqlab1/Assessment/Integrated_Report/IR_Database/IR_2018_v2.accdb", case="nochange")
#   
#   
#   
#   # Get data from IR database where wqst = 16 and ResStatusName = Final
#   # Join with Crit_ToxHH to get ToxHH Criteria
#   Results_import <-
#     sqlQuery(
#       IR.sql,
#       "SELECT InputRaw.OrgID, InputRaw.MLocID, InputRaw.AU_ID, InputRaw.HUC4_Name, InputRaw.MonLocType, InputRaw.TribalLand, InputRaw.wqstd_code, InputRaw.Pollu_ID, InputRaw.ChrName, InputRaw.ActMediaName, InputRaw.ActMediaSubName, InputRaw.SampleFractName, InputRaw.ResStatusName, InputRaw.[Pollutant_DEQ WQS], InputRaw.Result, InputRaw.Result4IR, InputRaw.ResultOp4IR, InputRaw.ResultUnitName, InputRaw.ResultComment, InputRaw.ResultMeasQualDesc, Crit_ToxHH.WaterOrganism, Crit_ToxHH.Organism, Crit_ToxHH.Organism_SW
# FROM InputRaw INNER JOIN Crit_ToxHH ON InputRaw.Pollu_ID = Crit_ToxHH.Pollu_ID
#       WHERE (((InputRaw.wqstd_code)=16) AND ((InputRaw.ResStatusName)='Final'));
#       ")
#   
#   
#   odbcClose(IR.sql)
#   
#   
  #######################################################
  ###         Temporary save of Results import -      ###
  ###         Remove this when more data is in db     ###
  #######################################################
  
  #save(Results_import, file = "Parameters/Tox_HH/Results.RData")
  
  #Load in saved dataset
  load("Parameters/Tox_HH/Results.RData")
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$STATION_KEY)), "monitoring locations" ))
  
  
  
  ############ - I did not adjust anything below here. 
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  # Get all the standards to be used when dealing with the censored data
  Results_crit <- Results_import %>%
    # Get lowest criteria value to set censored results
    mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  
  
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