## Lesley Merrick - 9/21/2018

require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)




Temp_data <- function(database) {
  print("Fetch Temperature data from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-  odbcConnectAccess2007(database, case="nochange")
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-
    sqlQuery(
      IR.sql,
      "SELECT InputRaw.OrgID, InputRaw.MLocID, InputRaw.AU_ID, InputRaw.HUC4_Name, InputRaw.MonLocType, InputRaw.TribalLand, InputRaw.wqstd_code, InputRaw.BacteriaCo, InputRaw.Pollu_ID, InputRaw.ChrName, InputRaw.ActMediaName, InputRaw.ActMediaSubName, InputRaw.SampleFractName, InputRaw.ResStatusName, InputRaw.[Pollutant_DEQ WQS], InputRaw.Result, InputRaw.Result4IR, InputRaw.ResultOp4IR, InputRaw.ResultUnitName, InputRaw.ResultComment, InputRaw.ResultlabComment, InputRaw.ResultMeasQualDesc, Crit_pH.pH_Min, Crit_pH.pH_Max
      FROM Crit_pH INNER JOIN InputRaw ON Crit_pH.pH_code = InputRaw.pH_code
      WHERE (((InputRaw.wqstd_code)=10) AND ((InputRaw.ResStatusName)='Final'));
      ")

  
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$STATION_KEY)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  print("Modify censored data")
  
  #run the censored data function to set censored data. This will use the lowest crit value from above 
  Results_censored <- Censored_data(Results_import, crit = `Temp_C` ) %>%
    mutate(Result_cen = as.numeric(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored%>%
    filter(!is.na(Result_cen))
  
  print("Data fetch and censored data modifications complete")
  
  return(Results_censored)
}