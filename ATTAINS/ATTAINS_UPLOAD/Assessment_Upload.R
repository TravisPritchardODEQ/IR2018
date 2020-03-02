### Lesley Merrick 2/26/2020
### building attains upload - ASSESSMENT 

library(tidyverse)
library(IRlibrary)
library(data.table)
require(RODBC)

#This script will generate the upload for the assessment portion of ATTAINS 

#this is test dataset filtered from \\deqhq1\WQASSESSMENT\2018IRFiles\2018_WQAssessment\Draft List\Rollup\Basin_categories 
# once finalized, this should be a table in the IR2018 database 
rollup <- read.csv("ATTAINS/ATTAINS_UPLOAD/Test_Rollup_Parameters.csv") %>%
          filter(AU_ID == 'OR_SR_1707030110_05_102624') ## remove this

# connect to IRdatabase  
IR.sql <-   odbcConnect("IR 2018")
Pollutant <-  sqlFetch(IR.sql, "dbo.LU_Pollutant") # make this is cleaned up 
LU_spawn <- sqlFetch(IR.sql, "dbo.LU_Spawn")
BU <- sqlFetch(IR.sql, "dbo.LU_BenUseCode") 
## make DB table? 
BU_2_Pollu <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/LU Bus.csv")
AU_tbl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/AU_names.csv")

#### Build Assessment Parameter table #### 
# need to add Delisting stuff
Param <- rollup %>% 
  filter(Assessed_in_2018 == "YES") %>%
  left_join(Pollutant, by = 'Pollu_ID') %>% 
  left_join(BU_2_Pollu, by = 'Pollu_ID') %>% 
  mutate(Para_Attainment = case_when(IR_category == 'Category 2' ~ "meeting criteria",
                                 IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4C') ~ "not meeting criteria",
                                 IR_category %in% c('Category 3','Category 3D','Category 3B','Category 3C') ~ "not enough information", 
                                 TRUE ~ "")) %>%
  mutate(Para_Status = case_when(IR_category == 'Category 2' ~ "Meeting Criteria",
                                     IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4B') ~ "Cause",
                                     TRUE ~ "")) %>%
  mutate(Param_Indicator = ifelse(IR_category == 'Category 4C', "N","Y")) %>%
  mutate(Param_trend = "U",
         PARAM_COMMENT = Rational,
         Para_agency_code = "S", 
         Param_Listed_year = Year_listed,
         PARAM_PRIORITY_RANKING = "Low") 
  
### filtered down table for upload 
Param_upload <- Param %>%
                select(AU_ID,Attains_PolluName,ben_use,Para_Status,Para_Attainment,Para_agency_code,
                       Param_Indicator,Param_Listed_year,PARAM_PRIORITY_RANKING,PARAM_COMMENT) %>%
                rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,
                       PARAM_STATUS_NAME = Para_Status,PARAM_ATTAINMENT_CODE=Para_Attainment,
                       PARAM_AGENCY_CODE=Para_agency_code,PARAM_POLLUTANT_INDICATOR=Param_Indicator,
                       PARAM_YEAR_LISTED=Param_Listed_year)
write.csv(Param_upload,"Param_102624.csv", row.names = FALSE) 

### build seasonal ####
seasons <- Param %>% 
           left_join(LU_spawn, by = "SpawnCode") %>% 
           select(AU_ID,Attains_PolluName,ben_use,Para_Attainment,SpawnStart,SpawnEnd) %>%
           rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,PARAM_ATTAINMENT_CODE=Para_Attainment,
                  SEASON_START=SpawnStart,SEASON_END=SpawnEnd)

write.csv(seasons,"Season_102624.csv", row.names = FALSE)            
  
#### Build ATTAINS Assessment- uses table####
# should we add additional data - monitoring location information ? 
Use <- Param %>%
  select(AU_ID) %>%
  distinct(AU_ID, .keep_all = TRUE) %>%  
  left_join(AU_tbl, by = 'AU_ID') %>%
  left_join(BU, by = c('AU_UseCode' = 'ben_use_code')) %>%
  left_join(Param, by = c('AU_ID','ben_use_id')) %>%
  group_by(AU_ID, ben_use.x) %>%
  summarise(total_samples = n(),
            num_impaired = sum(IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4C')),
            num_attaining = sum(IR_category == 'Category 2'),
            num_insuff = sum(IR_category %in% c('Category 3','Category 3D','Category 3B','Category 3C'))) %>%
  mutate(AU_Use_Status = case_when(num_impaired >= 1 ~ "N",
                                   num_attaining >= 1 & num_impaired == 0 ~ "F",
                                   num_insuff >= 1 & num_impaired == 0 & num_attaining == 0 ~ "I",
                                   TRUE ~ "X")) %>% 
  mutate(Use_agency_code = "S") %>%
  select(AU_ID,ben_use.x,AU_Use_Status,Use_agency_code) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,USE_NAME = ben_use.x, USE_ATTAINMENT_CODE = AU_Use_Status, USE_AGENCY_CODE = Use_agency_code)

write.csv(Use,"Use_102624.csv", row.names = FALSE) 



#### Build ATTAINS Assessment- general table####
## this may needs work for year last assessed and AU summary?
Gen <- Param %>%
  group_by(AU_ID) %>%
  mutate(keep = ifelse(year_assessed == max(year_assessed), 1, 0 )) %>%
  filter(keep == 1) %>%
  distinct(AU_ID, .keep_all = TRUE) %>%
  select(-keep) %>%
  mutate(CYCLE_LAST_ASSESSED = ifelse(year_assessed ==2018, 2020,"")) %>% # update 
  mutate(AGENCY_CODE = "S") %>% 
  select(AU_ID,AGENCY_CODE,CYCLE_LAST_ASSESSED) %>% 
  rename(ASSESSMENT_UNIT_ID = AU_ID)

write.csv(Gen,"Gen_102624.csv", row.names = FALSE)


