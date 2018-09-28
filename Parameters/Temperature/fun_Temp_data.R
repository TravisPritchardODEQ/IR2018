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
      "SELECT InputRaw.OrgID, InputRaw.MLocID, InputRaw.AU_ID, InputRaw.FishCode, InputRaw.SpawnCode, InputRaw.WaterTypeC, InputRaw.WaterBodyC, InputRaw.ben_use_co, InputRaw.HUC4_Name, InputRaw.MonLocType, InputRaw.wqstd_code, InputRaw.Pollu_ID, InputRaw.ChrName, InputRaw.ActMediaName, InputRaw.ActMediaSubName, InputRaw.ActStartD, InputRaw.ActStartT, InputRaw.ResStatusName, InputRaw.ResultBasesName, InputRaw.ResultTBaseName, InputRaw.Result, InputRaw.Result4IR, InputRaw.ResultOp4IR, InputRaw.ResultUnitName, InputRaw.ResultComment, InputRaw.ResultMeasQualDesc, LU_FishUse.FishUse_code, Crit_Temp.Temp_C, LU_Spawn.SpawnStart, LU_Spawn.SpawnEnd
FROM ((InputRaw INNER JOIN LU_Spawn ON InputRaw.SpawnCode = LU_Spawn.DO_SpawnCode) INNER JOIN LU_FishUse ON InputRaw.FishCode = LU_FishUse.FishUse_code) INNER JOIN Crit_Temp ON LU_FishUse.FishUse_code = Crit_Temp.FishUse_code
      WHERE (((InputRaw.wqstd_code)=12) AND ((InputRaw.ResStatusName)='Final') AND ((InputRaw.ResultBasesName)='7DADM'));
      ")

  
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
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