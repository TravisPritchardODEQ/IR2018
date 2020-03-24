library(tidyverse)
#library(IRlibrary)
library(openxlsx)
library(readxl)
library(data.table)
require(RODBC)

setwd("W:/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files")
impair_2012 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/impaired_2012.csv")
x_walk_impaired <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/x_walk_impaired.csv") %>%
  mutate(SUMMARY = trimws(SUMMARY, which = "both"))
AU_tbl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/AU_names.csv")
BU_2_Pollu <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/LU Bus.csv")
#clean version of the table fix in db
Pollutant <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/Attains_pollu_name.csv")
loaded  <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/ATTAINS/AUs_loaded.csv")
# connect to tables in IR database
#con <- DBI::dbConnect(odbc::odbc(), "IR 2018")
# Pollutant <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
#   mutate(Pollu_ID = as.character(Pollu_ID)) %>%
#   select(-SSMA_TimeStamp) %>%
#   mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right"))

#Pollutant <- sqlFetch(IR.sql, "dbo.LU_Pollutant") 
BU <- sqlFetch(IR.sql, "dbo.LU_BenUseCode") 

#### Build ATTAINS AU table####


AU <- AU_tbl %>% ## switching to find stations not loaded... x_walk_impaired %>%
      left_join(loaded, by = c('AU_ID' = 'ASSESSMENT_UNIT_ID')) %>% ##distinct(AU_ID, .keep_all = TRUE) %>% 
      filter(is.na(ASSESSMENT_UNIT_STATE)) %>%                                                        ###left_join(AU_tbl, by = 'AU_ID') %>% 
  mutate(state = "OR", Status = "A", Comment = '', locContext = "EPA",locunit = "HUC-12") %>% 
  mutate(watertype = case_when(grepl("^OR_CL",AU_ID ) ~ "COASTAL",
                               grepl("^OR_EB", AU_ID) ~ "ESTUARY",
                               grepl("^OR_LK", AU_ID) ~ "LAKE/RESERVOIR/POND",
                               grepl("^OR_WS", AU_ID) | grepl("^OR_SR", AU_ID) ~ "STREAM/CREEK/RIVER")) %>%
  mutate(waterunit = case_when(grepl("^OR_CL",AU_ID ) ~ "Miles",
                             grepl("^OR_EB", AU_ID) ~ "Square Miles",
                             grepl("^OR_LK", AU_ID) ~ "Acres",
                             grepl("^OR_WS", AU_ID) | grepl("^OR_SR", AU_ID) ~ "Miles")) %>%
  mutate(watersize = case_when(grepl("^OR_WS", AU_ID) | grepl("^OR_SR", AU_ID) | grepl("^OR_CL", AU_ID) ~ AU_LenMiles,
                                         grepl("^OR_EB", AU_ID) ~ (AU_AreaAcr*0.0015625) ,
                                         grepl("^OR_LK", AU_ID) ~ AU_AreaAcr)) %>%
  select(AU_ID,AU_Name,state,Status,Comment,AU_Description,watertype,watersize,waterunit) %>% # ,locContext,locunit,HUC12
  rename(ASSESSMENT_UNIT_ID = AU_ID , ASSESSMENT_UNIT_NAME = AU_Name, ASSESSMENT_UNIT_STATE = state,
         ASSESSMENT_UNIT_STATUS = Status, ASSESSMENT_UNIT_COMMENT = Comment,
        LOCATION_DESCRIPTION = AU_Description, WATER_TYPE = watertype,
         WATER_SIZE = watersize, WATER_UNIT = waterunit)
        #LOCATION_TYPE_CONTEXT = locContext,LOCATION_TYPE_CODE = locunit,
        #LOCATION_TEXT = HUC12)
klam <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/ATTAINS/AUs_KlamTMDL.csv") %>%
        left_join(AU, by= 'ASSESSMENT_UNIT_ID')

####### change name to the parameter being processed ######
write.csv(AU,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/ATTAINS/AUs_2load_4feb20.csv", row.names = FALSE)
write.csv(klam,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/ATTAINS/AUs_Klam_2load.csv", row.names = FALSE)
#### Build ATTAINS Monitoring location table####


#### Build Assessment Parameter table -  this seem like the best place to start #### 

Param <- x_walk_impaired %>% 
  left_join(Pollutant, by = 'Pollu_ID') %>% 
  left_join(BU_2_Pollu, by = 'Pollu_ID') %>% 
  mutate(Para_Status = "Cause") %>% # add other parameters ? 
  mutate(Param_Indicator = ifelse(STATUS_ID == 16, "N","Y")) %>%
  mutate(Para_Attainment = "Not meeting criteria", # change for full list 
         Param_trend = "U",
         PARAM_COMMENT = SUMMARY,
         Para_agency_code = ifelse(grepl("EPA addition", SUMMARY),"E","S"), 
         Param_Listed_year = ASSESSME_1,
         PARAM_PRIORITY_RANKING = "Medium") %>%
  select(AU_ID,Attains_PolluName,ben_use,Period,Pollu_ID,Para_Status,Para_Attainment,Para_agency_code,
       Param_Indicator,ASSESSME_1,PARAM_PRIORITY_RANKING,PARAM_COMMENT) 


YR_maxdate <- Param %>%
  filter(!Pollu_ID %in% c(154,132)) %>%
  group_by(AU_ID,Attains_PolluName,ben_use) %>%
  arrange(AU_ID,Attains_PolluName,ben_use) %>%
  mutate(keep = ifelse(ASSESSME_1 == max(ASSESSME_1), 1, 0 )) %>%
  filter(keep == 1) %>%
  distinct(AU_ID,Attains_PolluName,ben_use, .keep_all = TRUE) %>%
  select(-keep)

Seasonal_maxdate <- Param %>%
  filter(Pollu_ID %in% c(154,132)) %>%
  group_by(AU_ID,Attains_PolluName,ben_use) %>%
  arrange(AU_ID,Attains_PolluName,ben_use) %>%
  mutate(keep = ifelse(ASSESSME_1 == max(ASSESSME_1), 1, 0 )) %>%
  filter(keep == 1) %>%
  distinct(AU_ID,Attains_PolluName,ben_use, .keep_all = TRUE) %>%
  select(-keep)

Param_distinct <- rbind(YR_maxdate,Seasonal_maxdate) %>%
  select(AU_ID,Attains_PolluName,ben_use,Para_Status,Para_Attainment,Para_agency_code,
         Param_Indicator,ASSESSME_1,PARAM_PRIORITY_RANKING, Period,Pollu_ID, PARAM_COMMENT) %>% 
  rename(ASSESSMENT_UNIT_ID = AU_ID, PARAM_NAME = Attains_PolluName,PARAM_USE_NAME = ben_use,
       PARAM_STATUS_NAME = Para_Status, PARAM_ATTAINMENT_CODE = Para_Attainment, 
       PARAM_AGENCY_CODE = Para_agency_code, PARAM_POLLUTANT_INDICATOR = Param_Indicator,
       PARAM_YEAR_LISTED = ASSESSME_1)

write.csv(Param_distinct,"Param_9oct192.csv", row.names = FALSE) 



###### Counts - run this before the select statement ######
Param_counts_nonseasonal <- Param %>%
  filter(!Pollu_ID %in% c(154,132)) %>%
  group_by(AU_ID,Attains_PolluName,ben_use,Period) %>%
  summarise(num_results = n())
write.csv(Param_counts_nonseasonal, "Param_counts_nonseasonal.csv")

Param_counts_seasonal <- Param %>%
  filter(Pollu_ID %in% c(154,132)) %>%
  group_by(AU_ID,Attains_PolluName,ben_use,Period) %>%
  summarise(num_results = n())
write.csv(Param_counts_seasonal, "Param_counts_seasonal.csv")

Seasonal <- Param %>% 
  filter(Pollu_ID %in% c(154,132)) %>%
  group_by(AU_ID,Attains_PolluName,ben_use) %>%
  count(Period) %>%
  spread(Period, n)
write.csv(Seasonal, "spawn_YR.csv")


  
#### Build ATTAINS Assessment- uses table####
Use <- x_walk_impaired %>%
  select(AU_ID) %>%
  distinct(AU_ID, .keep_all = TRUE) %>%  
  left_join(AU_tbl, by = 'AU_ID') %>%
  right_join(BU, by = c(AU_UseCode = 'ben_use_code')) %>%
  left_join(Param, by = c('AU_ID'= 'AU_ID','ben_use'='ben_use')) %>% 
  mutate(Use_agency_code = "S") %>% 
  mutate(AU_Use_Status = ifelse(Para_Status == "Cause","N","X")) %>%
  group_by(AU_ID,ben_use) %>%
  mutate(AU_Use_Status = ifelse(AU_Use_Status == "N","N","X")) %>%
  distinct(AU_ID,ben_use,AU_Use_Status, .keep_all = TRUE) %>%
  select(AU_ID,ben_use,AU_Use_Status,Use_agency_code) %>% 
  rename(ASSESSMENT_UNIT_ID = AU_ID, USE_NAME = ben_use, USE_ATTAINMENT_CODE = AU_Use_Status,
         USE_AGENCY_CODE = Use_agency_code) %>% 
  filter(!is.na(ASSESSMENT_UNIT_ID))  
Use$USE_ATTAINMENT_CODE <- case_when(is.na(Use$USE_ATTAINMENT_CODE) ~ "X", 
                 !is.na(Use$USE_ATTAINMENT_CODE) ~ Use$USE_ATTAINMENT_CODE)
write.csv(Use,"Use_7oct19.csv", row.names = FALSE) 



#### Build ATTAINS Assessment- general table####
# need to figure out agency listed and concatinating 
Gen <- Use <- x_walk_impaired %>%
  select(AU_ID,ASSESSME_1,SUMMARY) %>%
  #distinct(AU_ID, .keep_all = TRUE) %>% 
  left_join(AU_tbl, by = 'AU_ID') %>%
  group_by(AU_ID) %>%
  arrange(AU_ID) %>%
  mutate(keep = ifelse(ASSESSME_1 == max(ASSESSME_1), 1, 0 )) %>%
  filter(keep == 1) %>%
  distinct(AU_ID, .keep_all = TRUE) %>%
  select(-keep) %>%
  # This isn't working 
  mutate(agency = ifelse(grepl("EPA addition", SUMMARY),"E","S")) %>% 
  select(AU_ID,agency,ASSESSME_1) %>% 
  rename(ASSESSMENT_UNIT_ID = AU_ID, AGENCY_CODE = agency, CYCLE_LAST_ASSESSED = ASSESSME_1)
  
write.csv(Gen,"Gen_8oct19.csv", row.names = FALSE)



  
  
  









