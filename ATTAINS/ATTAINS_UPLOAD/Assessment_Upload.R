### Lesley Merrick 2/26/2020
### building attains upload - ASSESSMENT 

library(tidyverse)
library(IRlibrary)
library(data.table)
require(RODBC)
library(readxl)

#This script will generate the upload for the assessment portion of ATTAINS 

#this is test dataset filtered from \\deqhq1\WQASSESSMENT\2018IRFiles\2018_WQAssessment\Draft List\Rollup\Basin_categories 
# once finalized, this should be a table in the IR2018 database 
rollup <- read.csv("ATTAINS/ATTAINS_UPLOAD/ALL BASINS_Parameters.csv") %>%
          #filter(AU_ID == 'OR_LK_1705011006_05_100541')
          filter(OWRD_Basin == 'Owyhee' | OWRD_Basin == 'Umpqua') ## remove this

# connect to IRdatabase  
IR.sql <-   odbcConnect("IR 2018")
Pollutant <-  sqlFetch(IR.sql, "dbo.LU_Pollutant") # make this is cleaned up 
LU_spawn <- sqlFetch(IR.sql, "dbo.LU_Spawn")
BU <- sqlFetch(IR.sql, "dbo.LU_BenUseCode") 
## make DB table? 
BU_2_Pollu <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/LU Bus.csv")
AU_tbl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/AU_names.csv")  ## update with columbia slough split
LU_delist <- read_excel("ATTAINS/ATTAINS_UPLOAD/Delisting_reasons_attains.xlsx", sheet = "Sheet1")
delistings <- read.csv("ATTAINS/ATTAINS_UPLOAD/ALL BASINS_delistingsv8.csv") %>% 
              left_join(LU_delist, by= c('Reason_Code'='reason_code')) %>% 
              filter(!is.na(Reason_Code))
#### Build Assessment Parameter table #### 
# need to add Delisting stuff
Param <- rollup %>% 
  filter(Assessed_in_2018 == "YES") %>%
  left_join(Pollutant, by = 'Pollu_ID') %>% 
  left_join(BU_2_Pollu, by = c('Pollu_ID','WQstd_code')) %>% 
  mutate(Para_Attainment = case_when(IR_category == 'Category 2' ~ "meeting criteria",
                                 IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4C') ~ "not meeting criteria",
                                 IR_category %in% c('Category 3','Category 3D','Category 3B','Category 3C') ~ "not enough information", 
                                 TRUE ~ "")) %>%
  mutate(Para_Status = case_when(IR_category == 'Category 2' ~ "Meeting Criteria",
                                     IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4B') ~ "Cause",
                                     TRUE ~ "")) %>%
  mutate(Param_Indicator = ifelse(IR_category %in% c('Category 2','Category 4C','Category 3','Category 3D',
                                                   'Category 3B','Category 3C'),"N","Y")) %>%
  mutate(Param_trend = "U",
         PARAM_COMMENT = Rationale,
         Para_agency_code = "S", 
         Param_Listed_year = Year_listed,
         PARAM_PRIORITY_RANKING = "Low") %>% ### change to update based on TMDL table 
  left_join(delistings, by = c('AU_ID','Pollu_ID','WQstd_code','Period')) %>% 
  distinct()
  

### build seasonal #### 
# must first run AU_Spawn_dates.R
Temp_s <- Param %>% 
  filter(Char_Name == 'Temperature' & Period =='Spawning') 

Temp_spawn_Start <- Temp_s %>%
  left_join(AU_Temp_Spawn, by = "AU_ID") %>%
  filter(!is.na(Temp_SpawnStart)) %>%
  select(AU_ID, Temp_SpawnStart,start_rank) %>%
  group_by(AU_ID) %>% 
  mutate(min_start = ifelse(start_rank == min(start_rank), Temp_SpawnStart, "X")) %>%
  filter(!min_start == 'X') %>%
  distinct()

Temp_spawn_End <- Temp_s %>%
  left_join(AU_Temp_Spawn, by = "AU_ID") %>%
  filter(!is.na(Temp_SpawnStart)) %>%
  select(AU_ID,Temp_SpawnEnd,end_rank) %>%
  group_by(AU_ID) %>% 
  mutate(max_end = ifelse(end_rank == max(end_rank), Temp_SpawnEnd, "X")) %>%
  filter(!max_end == 'X') %>%
  distinct()

Temp_Spawn <- Temp_s %>% 
  left_join(Temp_spawn_Start, by = 'AU_ID') %>% 
  left_join(Temp_spawn_End, by = 'AU_ID') %>% 
  select(AU_ID,Attains_PolluName,ben_use,Para_Attainment,min_start,max_end) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,PARAM_ATTAINMENT_CODE=Para_Attainment,
  SEASON_START=min_start,SEASON_END=max_end)

Temp_y <- Param %>% 
  filter(Char_Name == 'Temperature'& Period =='Year Round') %>% 
  mutate(min_start = "1/1") %>%
  mutate(max_end = "12/31") %>%
  select(AU_ID,Attains_PolluName,ben_use,Para_Attainment,min_start,max_end) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,
         PARAM_ATTAINMENT_CODE=Para_Attainment,
         SEASON_START=min_start,SEASON_END=max_end)

# Same process for DO   
DO_s <- Param %>% 
  filter(Char_Name == 'Dissolved Oxygen' & Period =='Spawning') 

DO_spawn_Start <- DO_s %>%
  left_join(AU_DO_Spawn, by = "AU_ID") %>%
  filter(!is.na(DO_SpawnStart)) %>%
  select(AU_ID, DO_SpawnStart,start_rank) %>%
  group_by(AU_ID) %>% 
  mutate(min_start = ifelse(start_rank == min(start_rank), DO_SpawnStart, "X")) %>%
  filter(!min_start == 'X') %>%
  distinct()

DO_spawn_End <- DO_s %>%
  left_join(AU_DO_Spawn, by = "AU_ID") %>%
  filter(!is.na(DO_SpawnStart)) %>%
  select(AU_ID,DO_SpawnEnd,end_rank) %>%
  group_by(AU_ID) %>% 
  mutate(max_end = ifelse(end_rank == max(end_rank), DO_SpawnEnd, "X")) %>%
  filter(!max_end == 'X') %>%
  distinct()

DO_Spawn <- DO_s %>% 
  left_join(DO_spawn_Start, by = 'AU_ID') %>% 
  left_join(DO_spawn_End, by = 'AU_ID') %>% 
  select(AU_ID,Attains_PolluName,ben_use,Para_Attainment,min_start,max_end) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,PARAM_ATTAINMENT_CODE=Para_Attainment,
         SEASON_START=min_start,SEASON_END=max_end)

DO_y <- Param %>% 
  filter(Char_Name == 'Dissolved Oxygen'& Period =='Year Round') %>% 
  mutate(min_start = "1/1") %>%
  mutate(max_end = "12/31") %>%
  select(AU_ID,Attains_PolluName,ben_use,Para_Attainment,min_start,max_end) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,PARAM_ATTAINMENT_CODE=Para_Attainment,
         SEASON_START=min_start,SEASON_END=max_end)
Season <- rbind(Temp_Spawn,Temp_y,DO_Spawn,DO_y) %>% distinct()

write.csv(Season,"ATTAINS/ATTAINS_UPLOAD/Season_owyhee_umpqua.csv", row.names = FALSE) 

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

### filtered down table for upload and checked for designated uses
Param_upload <- Param %>%
  left_join(Use, by = c('AU_ID' = 'ASSESSMENT_UNIT_ID','ben_use' = 'USE_NAME')) %>%
  filter(!is.na(USE_ATTAINMENT_CODE)) %>%
  select(AU_ID,Attains_PolluName,ben_use,Para_Status,Para_Attainment,Para_agency_code,
         Param_Indicator,Param_Listed_year,PARAM_PRIORITY_RANKING,PARAM_COMMENT,
         'Delisting Reason Code',Rationale.y) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=ben_use,
         PARAM_STATUS_NAME = Para_Status,PARAM_ATTAINMENT_CODE=Para_Attainment,
         PARAM_AGENCY_CODE=Para_agency_code,PARAM_POLLUTANT_INDICATOR=Param_Indicator,
         PARAM_YEAR_LISTED=Param_Listed_year,PARAM_DELISTING_REASON ='Delisting Reason Code',
         PARAM_DELISTING_COMMENT = Rationale.y) %>%
  mutate(PARAM_DELISTING_AGENCY = "S")


write.csv(Use,"ATTAINS/ATTAINS_UPLOAD/Use_owyhee_umpqua.csv", row.names = FALSE) 
write.csv(Param_upload,"ATTAINS/ATTAINS_UPLOAD/Param_owyhee_umpqua.csv", row.names = FALSE) 


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

write.csv(Gen,"ATTAINS/ATTAINS_UPLOAD/Gen_owyhee_umpqua.csv", row.names = FALSE)


