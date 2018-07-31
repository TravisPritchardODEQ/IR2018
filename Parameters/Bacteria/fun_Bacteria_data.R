

Bacteria_data <- function(database) {
  
  

  print("Fetch bacteria data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  IR.sql <-  odbcConnectAccess2007(database, case="nochange")
  
  
  
  # Get data from IR database where wqstd_code = 1 and ResStatusName = Final
  # Join with Crit_Bact to get bacteria Criteria
  Results_import <-
    sqlQuery(
      IR.sql,
      "SELECT InputRaw.STATION_KEY, InputRaw.MonLoc_Name, InputRaw.HUC8, InputRaw.Elev, InputRaw.AU_ID, InputRaw.FishCode, InputRaw.SpawnCode, InputRaw.WaterTypeC, InputRaw.WaterBodyC, InputRaw.BacteriaCo, InputRaw.ben_use_co, InputRaw.HUC4_name, InputRaw.wqstd_code, InputRaw.Pollu_ID, InputRaw.[Pollutant_DEQ WQS], InputRaw.OrgUID, InputRaw.OrgID, InputRaw.OrgName, InputRaw.MLocUID, InputRaw.MLocID, InputRaw.MLocName, InputRaw.MTypeUID, InputRaw.MTypeName, InputRaw.ActUID, InputRaw.ActID, InputRaw.ActTypeName, InputRaw.ActMediaName, InputRaw.ActMediaSubName, InputRaw.ActStartD, InputRaw.ActStartT, InputRaw.ActDepth, InputRaw.ActDepthUnit, InputRaw.ResultDepth, InputRaw.ResultDepthUnit, InputRaw.SampleFractName, InputRaw.ChrUID, InputRaw.ChrName, InputRaw.MethodSpecName, InputRaw.ResStatusName, InputRaw.ResultUID, InputRaw.Result, InputRaw.Result4IR, InputRaw.ResultOp4IR, InputRaw.ResultUnitUID, InputRaw.ResultUnitName, InputRaw.MDL, InputRaw.MDLunit, InputRaw.MRL, InputRaw.MRLUnit, InputRaw.DL4IR, InputRaw.ResultDetCondName, InputRaw.ResultBasesName, InputRaw.ResultTBaseName, InputRaw.AnalMethodName, InputRaw.ResultComment, InputRaw.ResultlabComment, InputRaw.ResultMeasQualID, InputRaw.ResultMeasQualDesc, InputRaw.res_statistic_n_value, InputRaw.act_sam_compnt_name, InputRaw.stant_name, InputRaw.res_wqx_submit_date, Crit_Bact.SS_Crit, Crit_Bact.Geomean_Crit, Crit_Bact.Perc_Crit
        FROM InputRaw INNER JOIN Crit_Bact ON InputRaw.BacteriaCo = Crit_Bact.BacteriaCode
        WHERE (((InputRaw.wqstd_code)=1) AND ((InputRaw.ResStatusName)='Final'));")
  
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$STATION_KEY)), "monitoring locations" ))
  
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