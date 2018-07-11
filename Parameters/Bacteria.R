require(rgdal)
require(RODBC)
library(tidyverse)

#disable scientific notation 
options(scipen = 999)


#connect to IR database view as a general user 
IR.sql <-  odbcConnectAccess2007("A:/Integrated_Report/IR_Database/IR_2018.accdb", case="nochange")

Results_import <- sqlQuery(IR.sql, 
"SELECT MS_TEST_MC.STATION_KEY, MS_TEST_MC.MonLoc_Name, MS_TEST_MC.HUC8, MS_TEST_MC.Elev, MS_TEST_MC.AU_ID, MS_TEST_MC.FishCode, MS_TEST_MC.SpawnCode, MS_TEST_MC.WaterTypeC, MS_TEST_MC.WaterBodyC, MS_TEST_MC.BacteriaCo, MS_TEST_MC.DO_code, MS_TEST_MC.ben_use_co, MS_TEST_MC.pH_code, MS_TEST_MC.HUC4_name, Pollutant_Info.wqstd_code, Pollutant_Info.Pollu_ID, Pollutant_Info.[Pollutant_DEQ WQS], dbo_resultsrawWATER.*
FROM ((MS_TEST_MC INNER JOIN dbo_resultsrawWATER ON MS_TEST_MC.STATION_KEY = dbo_resultsrawWATER.MLocID) INNER JOIN dbo_CharacteristicsForIR ON dbo_resultsrawWATER.ChrUID = dbo_CharacteristicsForIR.chr_uid) INNER JOIN Pollutant_Info ON dbo_CharacteristicsForIR.Pollu_ID = Pollutant_Info.Pollu_ID
WHERE (((Pollutant_Info.wqstd_code)=1));")



odbcClose(IR.sql)

# Set factors to characters
Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
