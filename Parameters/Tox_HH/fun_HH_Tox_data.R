require(rgdal)
require(RODBC)
require(tidyverse)
require(IRlibrary)

HH_tox_data <- function(database) {
  
  

  print("Fetch HH Tox data from IR database")
# connect to IR database view as a general user
# import TOXHH data
  
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_ToxHH") 
  
  
  odbcClose(IR.sql)
 
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  # choose the correct crit
  # if the ben-use code includes public or private water supply, select WaterOrganism
  # if ben-use does not include  public or private water supply, but does have fishing,  select Organism
  # if the characteristic has a salt water specific criteria, and the Water body code inidates salt water, 
  # and there is no drinking water, select Organism_SW 
  Results_import_crit <-  Results_import %>%
    mutate(crit = ifelse(ben_use_code %in% c('2','4','5','10','11','12', '16', '88'), WaterOrganism, 
                         ifelse(ben_use_code %in% c('1','3','6','7','8','9','13', '14', '15'), Organism, NA )),
           crit = ifelse(WaterBodyCode %in% c('1','3','4') & 
                           ben_use_code %in% c('1','3','6','7','8','9','13', '14', '15')&
                           !is.na(Organism_SW), Organism_SW, crit ))
  
  
  
  print("Modify censored data")
  
  #run the censored data function to set censored data. This will use the lowest crit value from above
  Results_censored <- Censored_data(Results_import_crit, crit = `crit` ) %>%
    mutate(Result_cen = as.numeric(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored %>%
    filter(!is.na(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$crit)), "Results with no criteria"))
  
  Results_censored <- Results_censored %>%
    filter(!is.na(crit))
  
  
  print("Data fetch and censored data modifications complete")

return(Results_censored)
  
}