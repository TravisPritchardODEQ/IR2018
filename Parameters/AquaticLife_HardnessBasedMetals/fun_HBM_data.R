require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)




HBM_data <- function(database) {
  print("Fetch hardness based metal data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  IR.sql <-  odbcConnectAccess2007(database, case="nochange")
  
  # Get data from IR database where wqstd_code = 1 and ResStatusName = Final
  # Join with Crit_Bact to get bacteria Criteria
  Results_import <-
    sqlQuery(IR.sql,
              "SELECT MS_TEST_MC.STATION_KEY, MS_TEST_MC.MonLoc_Name, MS_TEST_MC.AU_ID, MS_TEST_MC.WaterTypeC, MS_TEST_MC.WaterBodyC, MS_TEST_MC.ben_use_co, Pollutant_Type.wqstd_code, Pollutant_Info.Pollu_ID, Pollutant_Info.[Pollutant_DEQ WQS], dbo_resultsrawWATER.*
               FROM Pollutant_Type INNER JOIN (((MS_TEST_MC INNER JOIN dbo_resultsrawWATER ON MS_TEST_MC.STATION_KEY = dbo_resultsrawWATER.MLocID) INNER JOIN dbo_CharacteristicsForIR ON dbo_resultsrawWATER.ChrUID = dbo_CharacteristicsForIR.chr_uid) INNER JOIN Pollutant_Info ON dbo_CharacteristicsForIR.Pollu_ID = Pollutant_Info.Pollu_ID) ON (Pollutant_Info.Pollu_ID = Pollutant_Type.Pollu_ID) AND (Pollutant_Type.Pollu_ID = dbo_CharacteristicsForIR.Pollu_ID)
             WHERE (((Pollutant_Info.Pollu_ID)=25)) OR (((Pollutant_Info.Pollu_ID)=42)) OR (((Pollutant_Info.Pollu_ID)=101)) OR (((Pollutant_Info.Pollu_ID)=111)) OR (((Pollutant_Info.Pollu_ID)=130)) OR (((Pollutant_Info.Pollu_ID)=5));")
  
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$STATION_KEY)), "monitoring locations" ))
  