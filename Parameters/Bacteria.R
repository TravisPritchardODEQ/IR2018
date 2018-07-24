require(rgdal)
require(RODBC)
library(tidyverse)

#disable scientific notation 
options(scipen = 999)


#connect to IR database view as a general user 
IR.sql <-  odbcConnectAccess2007("A:/Integrated_Report/IR_Database/IR_2018.accdb", case="nochange")

Results_import <-
  sqlQuery(
    IR.sql,
    "SELECT InputRaw.STATION_KEY, InputRaw.MonLoc_Name, InputRaw.HUC8, InputRaw.Elev, InputRaw.AU_ID, InputRaw.FishCode, InputRaw.SpawnCode, InputRaw.WaterTypeC, InputRaw.WaterBodyC, InputRaw.BacteriaCo, InputRaw.ben_use_co, InputRaw.HUC4_name, InputRaw.wqstd_code, InputRaw.Pollu_ID, InputRaw.[Pollutant_DEQ WQS], InputRaw.OrgUID, InputRaw.OrgID, InputRaw.OrgName, InputRaw.MLocUID, InputRaw.MLocID, InputRaw.MLocName, InputRaw.MTypeUID, InputRaw.MTypeName, InputRaw.ActUID, InputRaw.ActID, InputRaw.ActTypeName, InputRaw.ActMediaName, InputRaw.ActMediaSubName, InputRaw.ActStartD, InputRaw.ActStartT, InputRaw.ActDepth, InputRaw.ActDepthUnit, InputRaw.ResultDepth, InputRaw.ResultDepthUnit, InputRaw.SampleFractName, InputRaw.ChrUID, InputRaw.ChrName, InputRaw.MethodSpecName, InputRaw.ResStatusName, InputRaw.ResultUID, InputRaw.Result, InputRaw.Result4IR, InputRaw.ResultOp4IR, InputRaw.ResultUnitUID, InputRaw.ResultUnitName, InputRaw.MDL, InputRaw.MDLunit, InputRaw.MRL, InputRaw.MRLUnit, InputRaw.DL4IR, InputRaw.ResultDetCondName, InputRaw.ResultBasesName, InputRaw.ResultTBaseName, InputRaw.AnalMethodName, InputRaw.ResultComment, InputRaw.ResultlabComment, InputRaw.ResultMeasQualID, InputRaw.ResultMeasQualDesc, InputRaw.res_statistic_n_value, InputRaw.act_sam_compnt_name, InputRaw.stant_name, InputRaw.res_wqx_submit_date, Crit_Bact.SS_Crit, Crit_Bact.Geomean_Crit, Crit_Bact.Perc_Crit
FROM InputRaw INNER JOIN Crit_Bact ON InputRaw.BacteriaCo = Crit_Bact.BacteriaCode
    WHERE (((InputRaw.wqstd_code)=1) AND ((InputRaw.ResStatusName)='Final'));")


odbcClose(IR.sql)

# Set factors to characters
Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import

# Perform censored data modifications
Results_censored <- Results_import %>%
  # Get lowest criteria value to set censored results
  mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE)) %>%
  

#create lists to get data out of for loops
geomeanlist = list()

