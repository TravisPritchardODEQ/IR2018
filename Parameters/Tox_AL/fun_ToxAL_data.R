require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)

#This function does the straight AL tixics analysis. It is for the parameters that have non-calculated standards, and do not
# need to be grouped and summed. 

#testing 
database = "IR 2018"
options(scipen = 999)

#ALTox_data <- function(database) {
  print("Fetch AL Toxic data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  IR.sql <-  odbcConnect(database, case="nochange")
  
  
  
  # Get data from IR database where wqstd_code = 15, pollutant is not DDT or PCB 
  #(excludes Pollu_ID 50,49,and 48 for DDT and 153 for PCB) and where sample fraction
  # is not suspended (removed suspended mercury and P). Joins with Crit_ToxAL to join criteria
  # Join with Crit_Bact to get bacteria Criteria
  Results_import <- sqlFetch(IR.sql,"VW_ToxAL")
  
  odbcClose(IR.sql)
 
   print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  # Data validation ---------------------------------------------------------
  
  print("No data validation")
  

  # Censored data ------------------------------------------------------------
  
  
  
  print("Modify censored data")
  
  # Create a variable for performing the censored_data analysis. If WaterTypeCode indicated freshwater, or is NA
  # then use the Chronic_FW as the criteria to base the data censoring off of. Otherwise, use saltwater chronic
  Results_import <- Results_import %>%
    mutate(censored_crit = ifelse((WaterTypeCode == 2 | is.na(WaterTypeCode)) & !is.na(Chronic_FW) , Chronic_FW, 
                                  ifelse((WaterTypeCode == 2 | is.na(WaterTypeCode)) & is.na(Chronic_FW), Acute_FW, 
                                         ifelse(WaterTypeCode != 2 & !is.na(Chronic_SW), Chronic_SW, Acute_SW ))))
  
  #run the censored data function to set censored data. This will use the lowest crit value from above 
  Results_censored <- Censored_data(Results_import, crit = `censored_crit` ) %>%
    mutate(Result_cen = as.numeric(Result_cen)) %>%
    select(-censored_crit)
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored %>%
    filter(!is.na(Result_cen))
  
  print("Data fetch and censored data modifications complete")
  
  #return(Results_censored)
#}  
  