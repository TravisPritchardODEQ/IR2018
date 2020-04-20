library(tidyverse)



# parameters --------------------------------------------------------------


load("ATTAINS/ATTAINS QC/rollup_data_for_QC.Rdata")

all_BU_rollup <- BU_Summary_4_QC %>%
  filter(!is.na(AU_Name)) %>%
  ungroup()

ATTAINS_parameters <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/ATTAINS_DOWNLOAD/april_16_2020/parameters.csv",
                               stringsAsFactors = FALSE)


BU_QC_original <- all_BU_rollup %>%
  select(AU_ID, Char_Name, Pollu_ID, WQstd_code, Period, IR_category, ben_use) %>%
  left_join(select(Pollutants, Pollu_ID, Attains_PolluName), by = "Pollu_ID") %>%
  filter(!is.na(Attains_PolluName)) %>%
  #Match Param use names with attains style
  mutate(PARAM_USE_NAME = case_when(ben_use == "Fishing" ~ "fishing",
                                    ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                                    ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                                    ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                                    ben_use == "Water Contact Recreation" ~ "water contact recreation",
                                    ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                                    ben_use == "Livestock Watering" ~ "livestock watering",
                                    ben_use == "Boating" ~ "boating",
                                    TRUE ~ "ERROR"
                                    )) %>%
  mutate(PARAM_USE_NAME = ifelse(PARAM_USE_NAME == "ERROR", NA, PARAM_USE_NAME )) %>%
  mutate(PARAM_USE_NAME = ben_use) %>%
  mutate(PARAM_USE_NAME = ifelse(Period == "Spawning" & !is.na(Period), paste(PARAM_USE_NAME, "- spawning"), PARAM_USE_NAME )) %>%
  #match status name
  mutate(PARAM_STATUS_NAME = case_when(grepl('5|4', IR_category) ~ 'Cause',
                                       grepl('3', IR_category) ~ 'Insufficient Information',
                                       grepl('2', IR_category) ~ 'Meeting Criteria',
                                       TRUE ~ 'ERROR')) %>%
  filter(PARAM_STATUS_NAME != 'ERROR') %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         PARAM_NAME = Attains_PolluName,
         PARAM_STATUS_NAME_original = PARAM_STATUS_NAME) %>%
  select(ASSESSMENT_UNIT_ID,
         PARAM_NAME,
         PARAM_USE_NAME,
         PARAM_STATUS_NAME_original,
         IR_category,
         Period) %>%
  mutate(PARAM_NAME = str_replace_all(PARAM_NAME, "[\\\r\\\n]" , ""))

BU_QC <- ATTAINS_parameters %>%
  full_join(BU_QC_original) %>%
  mutate(match = ifelse(PARAM_STATUS_NAME == PARAM_STATUS_NAME_original, 1, 0 ))


BU_QC_Errors <- BU_QC %>%
  filter(match != 1 | is.na(match))


write.xlsx(BU_QC_Errors, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/ATTAINS_DOWNLOAD/QC/parameter_errors2.xlsx")

Habitat_QC <- 



crosswalk <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Crosswalk/2012Crosswalk_Final.csv",
               stringsAsFactors = FALSE)


paramter_BUs_missing_from_attains <- BU_QC_Errors %>%
  filter(!is.na(IR_category) & is.na(PARAM_STATUS_NAME))

parameter_BUs_not_in_rollup <- BU_QC_Errors %>%
  filter(is.na(IR_category))



# Uses --------------------------------------------------------------------


