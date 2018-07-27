require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)

#disable scientific notation 
options(scipen = 999)



Coastal_Contact_rec <- function(){
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
  
  
  # Get all the standard to be used when dealing with the censored
  Results_crit <- Results_import %>%
    # Get lowest criteria value to set censored results
    mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  
  
  Results_censored <- Censored_data(Results_crit, crit = `lowest_crit` ) %>%
    mutate(Result_cen = as.numeric(Result_cen)) %>%
    filter(!is.na(Result_cen))
  
  
  #create lists to get data out of for loops
  geomeanlist = list()
  
  
  
  # Water Contact Recreation - Coastal -----------------------------------
  
  Coastal <- Results_censored %>%
    filter(BacteriaCo == 2,
           ChrName == "Enterococcus") %>%
    mutate(geomean = "",
           count_period = "",
           n_above_crit = "",
           perc_above_crit_10 = "",
           perc_above_crit_5 = "",
           less_5 = "",
           Max_value = "")
  
  # Geometric mean calculations --------------------------------------------
  
  
  # Process the geometirc means
  # These for loops first filter data down to individual monitoring stations
  # and sets a variable for each sampling date that indicates the start of a 90 day geomean window.
  # The second for loop loops through each activity date and creates a table of all activity dates in that
  # 90 day window and calculates the geomettric mean. It then assigns the geomeans into the single location table
  # created in the first loop, if there are more than 5 sampling dates in that window. 
  # The end of the first loop puts the single location table into a list which is used to bring
  # the data out of the for loop by binding it together after the loop into table "ecoli_geomean"
  
  for(i in 1:length(unique(Coastal$AU_ID))){
    
    station <- unique(Coastal$AU_ID)[i]
    
    # Filter table down to single station
    Coastal_singlestation <- Coastal %>%
      filter(AU_ID == station) %>%
      mutate(geomean_start_date = as.Date(ActStartD)-90)
    
    for(j in 1:nrow(Coastal_singlestation)){
      
      #start of 90 day window
      geomean_date <- Coastal_singlestation$geomean_start_date[j]
      # end of 90 day window
      enddate <- Coastal_singlestation$ActStartD[j]
      
      #create table for only samples in that window
      fresh_90_period <- Coastal_singlestation %>%
        filter(ActStartD <= enddate & ActStartD >= geomean_date )
      
      count_period = nrow(fresh_90_period)
      
      #get geomeans if number of samples in that window is 5 or greater
      Coastal_singlestation[j,"geomean"] <- ifelse(nrow(fresh_90_period) >= 5, geo_mean(fresh_90_period$Result_cen), NA)
      #get count of 90 day period
      Coastal_singlestation[j,"count_period"] <- count_period
      # get number that are above 130 criterion 
      Coastal_singlestation[j,"n_above_crit"] <- sum(fresh_90_period$Result_cen > 130) 
      # get percent that are above criteria if more than 10 samples in 90 day period
      Coastal_singlestation[j,"perc_above_crit_10"] <- ifelse(count_period >= 10, n_above_crit/count_period, NA)
      # get lowest value in 90 day window if 5-9 samples in 90 day window
      Coastal_singlestation[j,"perc_above_crit_5"]  <- ifelse(count_period < 10 & count_period >= 5, max(fresh_90_period$Result_cen), NA )
      # flag if less than 5 in 90 day window
      Coastal_singlestation[j,"less_5"] <- ifelse(nrow(fresh_90_period) < 5, 1, 0)
      #Max Value
      Coastal_singlestation[j,"Max_value"] <- max(fresh_90_period$Result_cen)
      
      
    }
    
    geomeanlist[[i]] <- Coastal_singlestation
    
  }
  
  Coastal_analysis <- bind_rows(geomeanlist) %>%
    mutate(geomean = as.numeric(geomean),
           count_period = as.numeric(count_period),
           n_above_crit = as.numeric(n_above_crit),
           perc_above_crit_10 = as.numeric(perc_above_crit_10),
           perc_above_crit_5 = as.numeric(perc_above_crit_5 ),
           less_5 = as.numeric(less_5))
  
  
  
  Coastal_AU_summary <-  Coastal_analysis %>%
    group_by(AU_ID) %>%
    summarise(Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
              max.perc_above_crit_10 =  ifelse(!all(is.na(perc_above_crit_10)),max(perc_above_crit_10, na.rm = TRUE),NA),
              max.perc_above_crit_5 = ifelse(!all(is.na(perc_above_crit_5)),max(perc_above_crit_5, na.rm= TRUE),NA),
              perc.insuff = sum(less_5)/n(),
              max.value  = max(Result_cen)) %>%
    mutate(Cat5 = ifelse((!is.na(Max_Geomean) & Max_Geomean > 35) | 
                           (!is.na(max.perc_above_crit_10) & max.perc_above_crit_10 > 0.10) | 
                           (!is.na(max.perc_above_crit_5) & max.perc_above_crit_5 > 130), 1, 0),
           Cat3 = ifelse(Cat5 !=1 & perc.insuff == 1 & max.value < 130, 1, 0),
           Cat3B = ifelse(Cat5 !=1 & perc.insuff == 1 & max.value > 130, 1, 0),
           Cat2 = ifelse((
             !is.na(Max_Geomean) &
               Max_Geomean <= 35 &
               perc.insuff < 1 & 
               !is.na(max.perc_above_crit_10) & max.perc_above_crit_10 < 0.10) |
               (!is.na(Max_Geomean) &
                  Max_Geomean <= 35 &
                  perc.insuff < 1 &
                  !is.na(max.perc_above_crit_5) & max.perc_above_crit_5 < 130),1,0)
    )
  
  return(Coastal_AU_summary)
}
