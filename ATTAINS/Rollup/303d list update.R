# 303(d) list to send to EPA
library(tidyverse)
library(openxlsx)


# This script joins the monitoring locations to the BU_Summary table and saves
# the result table as joined_BU_summary.joined_BU_summary is the table that the 
# IR data display uses at it's data source.

# monitoring locations are taken from the ALLDATA data files in the 2018_WQAssessment/Draft List
# folder

options(scipen = 9999999)
con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

# Load assessment result data
all_bains_categories <- read.xlsx("ATTAINS/Rollup/Basin_categories/ALL BASINS_categories.xlsx")

# This table connects the Pollu_IDs and WQstrd codes to beneficial uses.
# This is how we assign uses to assessments
BUs <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU Bus.csv",
                stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = as.character(WQstd_code)) 


BUs_group <- BUs %>%
  group_by(WQstd_code, Pollu_ID) %>%
  summarise(Beneficial_uses = str_c(unique(ben_use), collapse = "; "))

all_bains_categories <- all_bains_categories %>%
  left_join(BUs_group,  by = c("Pollu_ID", "WQstd_code"))



Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right")) %>%
  select(Pollu_ID,Attains_PolluName )



all_bains_categories <- all_bains_categories %>%
  left_join(Pollutants) %>%
  mutate(Attains_PolluName = case_when(Attains_PolluName == "SEDIMENT\r\n" ~ "SEDIMENT",
                                       Attains_PolluName == "4,4'-DDT\r" ~ "4,4'-DDT",
                                       TRUE ~Attains_PolluName ))

#read in parameters.csv file from Lesley's email
parameters <- read.csv("ATTAINS/priority ranking for attains/parameters.csv",
                       stringsAsFactors = FALSE) %>%
  select(ASSESSMENT_UNIT_ID, PARAM_NAME, PARAM_PRIORITY_RANKING) %>%
  distinct()
  

all_bains_categories <- all_bains_categories %>%
  left_join(parameters, by = c("AU_ID" = "ASSESSMENT_UNIT_ID","Attains_PolluName" = "PARAM_NAME"  )) 



# Load in the ALLDATA files -----------------------------------------------

bacteria_coast_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Bacteria_Coast_Contact_IR_Data_ALLDATA.csv", 
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

bacteria_fresh_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Bacteria_Fresh_Contact_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

bacteria_Shell_harvest <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Bacteria_Shell_harvest_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

chl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Chla_IR_data_ALLDATA.csv",
                stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

DO_cont_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_Continuous_Spawn_IR_data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Spawning')

DO_cont_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_YearRound_continuous_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Year Round')

DO_instant_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_Instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Spawning')

DO_inst_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_YearRound_instant_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Year Round')

DO_estuary_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_estuary_instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code)%>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Spawning')

DO_estuary_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_Estuary_Yearround_IR_data_ALLDATA.csv",
                                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Year Round')


pH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/pH_IR_data_ALLDATA.csv",
               stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

temp_year_round <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Temperature_IR_data_ALLDATA.csv",
                            stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  mutate(Spawn_criteria = ifelse(Spawn_type == "Spawn", 13, "" ) ) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Year Round')

temp_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data//Temperature_IR_data_ALLDATA.csv",
                       stringsAsFactors = FALSE) %>%
  filter(Spawn_type == 'Spawn') %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  mutate(Spawn_criteria = ifelse(Spawn_type == "Spawn", 13, "" ) ) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Spawn')


Tox_AL_Ammonia <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Ammonia_IR_Data_ALLDATA.csv",
                           stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code,Result_UID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

Tox_AL_CU <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Cu_IR_Data_ALLDATA.csv",
                      stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code,Result_UID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

Tox_AL_Hardness_Metals <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Hardness_Metals_IR_Data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code, Result_UID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

Tox_AL_Others <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Others_IR_Data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code,Result_UID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) %>%
  mutate(Pollu_ID = case_when(Char_Name == 'Endosulfan'~ '77', 
                              Char_Name == "DDT" ~ '50', 
                              TRUE ~ Pollu_ID ))

Tox_AL_Penta <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Pentachlorophenol_IR_data_ALLDATA.csv",
                         stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code,Result_UID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


tox_AL_initial <- Tox_AL_Ammonia %>%
  bind_rows(Tox_AL_CU) %>%
  bind_rows(Tox_AL_Hardness_Metals) %>%
  bind_rows(Tox_AL_Others) %>%
  bind_rows(Tox_AL_Penta)




MonLocs <-  DBI::dbGetQuery(con, "SELECT [Result_UID],
                               [MLocID] as mon_station
                               FROM [IntegratedReport].[dbo].[InputRaw]") 

MonLocs <- distinct(MonLocs)


tox_AL <- tox_AL_initial %>%
  left_join(MonLocs) %>%
  mutate(MLocID = mon_station) %>%
  select(-mon_station, -Result_UID)



Tox_HH_initial <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Tox_HH_IR_data_ALLDATA.csv",
                           stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code,Result_UID ) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))







# HH_MonLocs <-  DBI::dbGetQuery(con, "SELECT [Result_UID],
# 	   [MLocID] as mon_station
#       
#   FROM [IntegratedReport].[dbo].[VW_ToxHH]")

Tox_HH <- Tox_HH_initial %>%
  left_join(MonLocs) %>%
  mutate(MLocID = mon_station) %>%
  select(-mon_station, -Result_UID)

Tox_HH_Hg_tissue <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Tox_HH_hg_tissue_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  mutate(Pollu_ID = '109',
         wqstd_code = '16') %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


biocriteria <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Biocriteria/Bio_3Dec18.xlsx",
                         sheet = 'Bio_Data') %>%
  mutate(Char_Name = "Biocriteria",
         Pollu_ID = '156',
         wqstd_code = '5') %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

# Get temperature cat 3 monitoiring locations -----------------------------

#query from the database locations that have continuous data



all_temperature_data <- "SELECT [AU_ID], 
OWRD_Basin, 
MLocID,
min([SampleStartDate]) as data_period_start, 
max([SampleStartDate]) as data_period_end

FROM [IntegratedReport].[dbo].[InputRaw]
where Char_Name = 'Temperature, water' and Statistical_Base = 'Mean'
group by [AU_ID], MLocID, OWRD_Basin
"

all_temp_AUs_query <-  glue::glue_sql(all_temperature_data, .con = con)
all_temp_AUs <- DBI::dbGetQuery(con, all_temp_AUs_query)
DBI::dbDisconnect(con)


# AU_S in the all_temp_AUs dataframe that are not in the temp_year_round df
# are the ones that have continuous data and no 7DADM data
# add columns to insert additional data.
AUs_with_no_7DADM <- all_temp_AUs %>%
  filter(!AU_ID %in% unique(temp_year_round$AU_ID),
         AU_ID != "") %>%
  mutate(wqstd_code = "12",
         Pollu_ID = "132",
         Char_Name = "Temperature, water") %>%
  select(AU_ID, MLocID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code),
         Period = 'Year Round')


temp_to_join <- AUs_with_no_7DADM %>%
  select(AU_ID, MLocID,  Pollu_ID, wqstd_code, Period) %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code, Period) %>%
  summarise(Monitoring_locations = str_c(unique(MLocID), collapse  = "; ")) %>%
  rename(WQstd_code = wqstd_code)

data_together <- bacteria_coast_contact %>%
  bind_rows(bacteria_fresh_contact) %>%  
  bind_rows(bacteria_Shell_harvest) %>%
  bind_rows(chl) %>%
  bind_rows(DO_cont_spawn) %>%
  bind_rows(DO_cont_yearround) %>%
  bind_rows(DO_instant_spawn) %>%
  bind_rows(DO_inst_yearround) %>%
  bind_rows(DO_estuary_spawn) %>%
  bind_rows(DO_estuary_yearround) %>%
  bind_rows(pH) %>%
  bind_rows(temp_year_round) %>%
  bind_rows(temp_spawn) %>%
  bind_rows(tox_AL) %>%
  bind_rows(Tox_HH) %>% 
  bind_rows(Tox_HH_Hg_tissue) %>%
  bind_rows(AUs_with_no_7DADM) %>%
  bind_rows(biocriteria)

data_to_join <- data_together %>%
  select(AU_ID, MLocID,  Pollu_ID, wqstd_code, Period) %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code, Period) %>%
  summarise(Monitoring_locations = str_c(unique(MLocID), collapse  = "; ")) %>%
  rename(WQstd_code = wqstd_code) %>%
  bind_rows(temp_to_join) %>%
  distinct()


list_303d <- all_bains_categories %>%
  mutate(Period = ifelse(Period == "Year round", "Year Round", Period )) %>%
  left_join(data_to_join, by = c("AU_ID", "Pollu_ID", "WQstd_code", "Period")) %>%
  mutate(Monitoring_locations = ifelse(AU_ID == 'OR_LK_1701030502_02_107194' & Char_Name == "Ammonia", '37053-ORDEQ', Monitoring_locations )) %>%
  mutate(Assessment = case_when(!is.na(Period) ~ paste0(Char_Name, "- ",Period),
                                WQstd_code == "15" ~ paste0(Char_Name, "- Aquatic Life Criteria"),
                                WQstd_code == "16" ~ paste0(Char_Name, "- Human Health Criteria"),
                                TRUE ~ Char_Name)) %>%
  select(AU_ID, AU_Name, AU_Description, OWRD_Basin, Char_Name,  
         Assessment,Period, IR_category, Monitoring_locations, Rationale, 
         Year_listed, Assessed_in_2018, Beneficial_uses, PARAM_PRIORITY_RANKING ) %>%
  mutate(Rationale = ifelse(IR_category ==  "Category 5" |  IR_category == "Category 4A" | 
                              IR_category == "Category 4"  | 
                              IR_category == "Category 4b" |  
                              IR_category == "Category 4C", Rationale, '' )) %>%
  mutate(Rationale = ifelse(is.na(Rationale), '', Rationale )) %>%
  mutate(Rationale = ifelse(Assessed_in_2018 == 'NO', "Carried forward from previous listing", Rationale )) %>%
  mutate(Monitoring_locations = ifelse(AU_ID == 'OR_SR_1710020608_02_105080' & is.na(Monitoring_locations), '33642-ORDEQ', Monitoring_locations )) %>%
  filter(grepl("4", IR_category) | grepl("5", IR_category)) %>%
  mutate(PARAM_PRIORITY_RANKING = ifelse(grepl("4", IR_category), "",PARAM_PRIORITY_RANKING ))


write.xlsx(list_303d, file = paste0("ATTAINS/Rollup/Basin_categories/303d_list_with_priorities-",Sys.Date(), ".xlsx"))

