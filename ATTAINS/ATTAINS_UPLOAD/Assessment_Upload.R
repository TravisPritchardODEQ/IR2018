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
rollup <- read.csv("~/IR2018/ATTAINS/ATTAINS_UPLOAD/ALL BASINS_Parameters.csv") #%>%
          #filter(AU_ID == 'OR_LK_1705011006_05_100541')
          #filter(OWRD_Basin == 'Owyhee' | OWRD_Basin == 'Umpqua') ## remove this

# connect to IRdatabase  
IR.sql <-   odbcConnect("IR 2018")
Pollutant <-  sqlFetch(IR.sql, "dbo.LU_Pollutant") # make this is cleaned up 
LU_spawn <- sqlFetch(IR.sql, "dbo.LU_Spawn")
BU <- sqlFetch(IR.sql, "dbo.LU_BenUseCode") 
## make DB table? 
BU_2_Pollu <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/LU Bus.csv")
AU_tbl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/AU_names.csv")  ## update with columbia slough split
LU_delist <- read_excel("~/IR2018/ATTAINS/ATTAINS_UPLOAD/Delisting_reasons_attains.xlsx", sheet = "Sheet1")
delistings <- read.csv("~/IR2018/ATTAINS/ATTAINS_UPLOAD/ALL BASINS_delistingsv8_attains.csv") %>% 
              left_join(LU_delist, by= c('Reason_Code'='reason_code')) %>% 
              filter(!is.na(Reason_Code))
TMDL <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/ATTAINS/Revised_uploads_7april2020/Cat4-5_AU_Priority_20200402.csv") %>%
  filter(TMDL_Priority %in% c('Medium','High')) %>% 
  select(AU_ID,PARAM_NAME,PARAM_USE_NAME,Period,TMDL_Priority)

#### Build Assessment Parameter table #### 

Param <- rollup %>% 
  #filter(Assessed_in_2018 == "YES") %>%
  left_join(Pollutant, by = 'Pollu_ID') %>% 
  left_join(BU_2_Pollu, by = c('Pollu_ID','WQstd_code')) %>% 
  left_join(TMDL, by = c('AU_ID','Attains_PolluName' = 'PARAM_NAME','ben_use'='PARAM_USE_NAME','Period')) %>% 
  mutate(TMDL_Priority = as.character(TMDL_Priority))  %>%
  mutate(PARAM_PRIORITY_RANKING = ifelse(is.na(TMDL_Priority), "Low",TMDL_Priority)) %>%
  mutate(ben_use = as.character(ben_use)) %>%
  mutate(attains_use = ifelse(Period == 'Spawning',"fish and aquatic life - spawning", ben_use)) %>%
  mutate(Para_Attainment = case_when(IR_category == 'Category 2' ~ "meeting criteria",
                                 IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4C') ~ "not meeting criteria",
                                 IR_category %in% c('Category 3','Category 3D','Category 3B','Category 3C') ~ "Not enough information", 
                                 TRUE ~ "")) %>%
  mutate(Para_Status = case_when(IR_category == 'Category 2' ~ "Meeting Criteria",
                                     IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4B') ~ "Cause",
                                 IR_category %in% c('Category 3','Category 3D','Category 3B','Category 3C') ~ "Insufficient Information",
                                     TRUE ~ "")) %>%
  mutate(Param_Indicator = ifelse(IR_category %in% c('Category 2','Category 4C','Category 3','Category 3D',
                                                   'Category 3B','Category 3C'),"N","Y")) %>%
  mutate(Param_trend = "U",
         PARAM_COMMENT = Rationale,
         Para_agency_code = ifelse(Para_Status == "Cause","S",""),
         Param_Listed_year = ifelse(year_assessed ==2018, 2020, Year_listed)) %>% ### remove this for 2022
  left_join(delistings, by = c('AU_ID','Pollu_ID','WQstd_code','Period')) %>% 
  distinct()
  

### build seasonal #### 
# must first run AU_Spawn_dates.R
Temp_s <- Param %>% 
  filter(Char_Name == 'Temperature' & Period =='Spawning') %>%
  filter(Assessed_in_2018 == "YES") 

Temp_spawn_Start <- Temp_s %>%
  left_join(AU_Temp_Spawn, by = "AU_ID") %>%
  filter(!is.na(Temp_SpawnStart)) %>%
  select(AU_ID, Temp_SpawnStart,start_rank, OWRD_Basin) %>%
  group_by(AU_ID) %>% 
  mutate(min_start = ifelse(start_rank == min(start_rank), Temp_SpawnStart, "X")) %>%
  filter(!min_start == 'X') %>%
  distinct()

Temp_spawn_End <- Temp_s %>%
  left_join(AU_Temp_Spawn, by = "AU_ID") %>%
  filter(!is.na(Temp_SpawnStart)) %>%
  select(AU_ID,Temp_SpawnEnd,end_rank,OWRD_Basin) %>%
  group_by(AU_ID) %>% 
  mutate(max_end = ifelse(end_rank == max(end_rank), Temp_SpawnEnd, "X")) %>%
  filter(!max_end == 'X') %>%
  distinct()

Temp_Spawn <- Temp_s %>% 
  left_join(Temp_spawn_Start, by = 'AU_ID') %>% 
  left_join(Temp_spawn_End, by = 'AU_ID') %>% 
  mutate(attains_use = "fish and aquatic life - spawning") %>% 
  select(AU_ID,Attains_PolluName,attains_use,Para_Attainment,min_start,max_end,OWRD_Basin) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=attains_use,PARAM_ATTAINMENT_CODE=Para_Attainment,
  SEASON_START=min_start,SEASON_END=max_end)

# Same process for DO   
DO_s <- Param %>% 
  filter(Char_Name == 'Dissolved Oxygen' & Period =='Spawning') %>%
  filter(Assessed_in_2018 == "YES")

DO_spawn_Start <- DO_s %>%
  left_join(AU_DO_Spawn, by = "AU_ID") %>%
  filter(!is.na(DO_SpawnStart)) %>%
  select(AU_ID, DO_SpawnStart,start_rank,OWRD_Basin) %>%
  group_by(AU_ID) %>% 
  mutate(min_start = ifelse(start_rank == min(start_rank), DO_SpawnStart, "X")) %>%
  filter(!min_start == 'X') %>%
  distinct()

DO_spawn_End <- DO_s %>%
  left_join(AU_DO_Spawn, by = "AU_ID") %>%
  filter(!is.na(DO_SpawnStart)) %>%
  select(AU_ID,DO_SpawnEnd,end_rank,OWRD_Basin) %>%
  group_by(AU_ID) %>% 
  mutate(max_end = ifelse(end_rank == max(end_rank), DO_SpawnEnd, "X")) %>%
  filter(!max_end == 'X') %>%
  distinct()

DO_Spawn <- DO_s %>% 
  left_join(DO_spawn_Start, by = 'AU_ID') %>% 
  left_join(DO_spawn_End, by = 'AU_ID') %>% 
  mutate(attains_use = "fish and aquatic life - spawning") %>% 
  select(AU_ID,Attains_PolluName,attains_use,Para_Attainment,min_start,max_end,OWRD_Basin) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=attains_use,PARAM_ATTAINMENT_CODE=Para_Attainment,
         SEASON_START=min_start,SEASON_END=max_end)

Season <- rbind(Temp_Spawn,DO_Spawn) %>% distinct()

write.csv(Season,"ATTAINS/ATTAINS_UPLOAD/Season_all.csv", row.names = FALSE) 

#### Build ATTAINS Assessment- uses table####
# should we add additional data - monitoring location information ? 
Use <- Param %>%
  filter(!Period == 'Spawning') %>%
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

## manually adding spawning use 
Use_spawn <- Param %>%
  filter(Period == 'Spawning') %>%
   mutate(USE_NAME = "fish and aquatic life - spawning") %>%
   group_by(AU_ID, USE_NAME) %>%
  summarise(total_samples = n(),
            num_impaired = sum(IR_category %in% c('Category 5','Category 4','Category 4A','Category 4B','Category 4C')),
            num_attaining = sum(IR_category == 'Category 2'),
            num_insuff = sum(IR_category %in% c('Category 3','Category 3D','Category 3B','Category 3C'))) %>%
  mutate(AU_Use_Status = case_when(num_impaired >= 1 ~ "N",
                                   num_attaining >= 1 & num_impaired == 0 ~ "F",
                                   num_insuff >= 1 & num_impaired == 0 & num_attaining == 0 ~ "I",
                                   TRUE ~ "X")) %>% 
  mutate(Use_agency_code = "S") %>%
  select(AU_ID,USE_NAME,AU_Use_Status,Use_agency_code) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID, USE_ATTAINMENT_CODE = AU_Use_Status, USE_AGENCY_CODE = Use_agency_code) %>%
  distinct()

# get OWRD basins 
AU_OWRD <- Param %>% 
  filter(Assessed_in_2018 == "YES") %>%
  select(AU_ID,OWRD_Basin) %>% 
  distinct()

Use_all <- rbind(Use,Use_spawn) 

Use_basin <- Use_all %>%
           left_join(AU_OWRD, by = c('ASSESSMENT_UNIT_ID' = 'AU_ID')) %>%
           distinct()
    
### filtered down table for upload and checked for designated uses
Param_upload <- Param %>%
 # filter(Assessed_in_2018 == "YES") %>%
  left_join(Use_all, by = c('AU_ID' = 'ASSESSMENT_UNIT_ID','attains_use' = 'USE_NAME')) %>%
  filter(!is.na(USE_ATTAINMENT_CODE)) %>%
  select(AU_ID,Attains_PolluName,attains_use,Para_Status,Para_Attainment,Para_agency_code,
         Param_Indicator,Param_Listed_year,PARAM_PRIORITY_RANKING,PARAM_COMMENT,
         'Delisting Reason Code',Rationale.y, OWRD_Basin) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName,PARAM_USE_NAME=attains_use,
         PARAM_STATUS_NAME = Para_Status,PARAM_ATTAINMENT_CODE=Para_Attainment,
         PARAM_AGENCY_CODE=Para_agency_code,PARAM_POLLUTANT_INDICATOR=Param_Indicator,
         PARAM_YEAR_LISTED=Param_Listed_year,
         PARAM_DELISTING_REASON ='Delisting Reason Code',
         PARAM_DELISTING_COMMENT = Rationale.y) %>%
  mutate(PARAM_DELISTING_AGENCY = "S") %>%
  filter(!is.na(PARAM_NAME))

Use_trim <- Use_basin %>% 
            left_join(Param_upload, by = c('ASSESSMENT_UNIT_ID','USE_NAME' = 'PARAM_USE_NAME')) %>%
            filter(!is.na(PARAM_NAME)) %>%
            select('ASSESSMENT_UNIT_ID','USE_NAME',USE_ATTAINMENT_CODE,USE_AGENCY_CODE,OWRD_Basin.x)


write.csv(Use_trim,"ATTAINS/ATTAINS_UPLOAD/Use_all2.csv", row.names = FALSE) 
write.csv(Param_upload,"ATTAINS/ATTAINS_UPLOAD/Param_all.csv", row.names = FALSE) 


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
  rename(ASSESSMENT_UNIT_ID = AU_ID) %>%
  left_join(AU_OWRD, by = c('ASSESSMENT_UNIT_ID' = 'AU_ID')) %>%
  distinct()


write.csv(Gen,"ATTAINS/ATTAINS_UPLOAD/Gen_all.csv", row.names = FALSE)

#### associated actions 

aa_all <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/ATTAINS/Revised_uploads_7april2020/associated-actions_new.csv")
aa_all_2 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/Action_AU_Parameter.csv")

a_actions <-  Param %>% 
  select(AU_ID,Attains_PolluName,Pollu_ID) %>%
  distinct() %>%
  left_join(aa_all_2, by = c('AU_ID','Pollu_ID')) %>% 
  filter(!is.na(ACTION_ID)) %>% 
  left_join(AU_OWRD, by = c('AU_ID')) %>%
  distinct() %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,PARAM_NAME=Attains_PolluName)

write.csv(a_actions,"ATTAINS/ATTAINS_UPLOAD/a_actions_all_2.csv", row.names = FALSE)
