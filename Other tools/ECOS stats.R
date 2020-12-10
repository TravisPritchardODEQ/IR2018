library(tidyverse)
library(openxlsx)

# read_in_assessments

assessments <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Rollup/ALL BASINS_categories.xlsx")


AU_status_overall <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Rollup/ALL BASINS_Impaired_1orMoreUses.xlsx")


Attaining <- AU_status_overall %>%
  filter(AU_status == 'Attaining')


Total_assessment_units <- 7245

total_assessed_units <- n_distinct(assessments$AU_ID)

Attaining_AUs <- n_distinct(Attaining$AU_ID)












########################################### data for by BU below here


BU_lookup <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU Bus.csv",
                      stringsAsFactors = FALSE) %>%
  mutate_if(is.integer, as.character)


Assess_BUs <- assessments %>%
  left_join(BU_lookup, by = c("Pollu_ID", "WQstd_code")) %>%
  filter(!is.na(Char_Name))


#Create ordered factor for IR_category 

Assess_BUs_factor <- factor(Assess_BUs$IR_category, levels = c('Unassigned',
                                                                     "-",
                                                                     "Category 3C",
                                                                     "Category 3D",
                                                                     "Category 3",
                                                                     "Category 3B",
                                                                     "Category 2",
                                                                     "Category 4A",
                                                                     "Category 5"),
                             ordered = TRUE)

Assess_BUs$IR_category <- Assess_BUs_factor


AU_BUs <- Assess_BUs %>%
  group_by(AU_ID, ben_use) %>%
  filter(IR_category == max(IR_category)) %>%
  ungroup() %>%
  mutate(status =  case_when(IR_category %in% c("Category 4B","Category 4C","Category 4A", "Category 5","Category 4" ) ~ "Impaired",
                         IR_category %in% c("Category 3C","Category 3D","Category 3B", "Category 3") ~ "Insufficient Data",
                         IR_category %in% c("Category 2","Category 3D","Category 3B") ~ "Attaining",
                         TRUE ~ 'ERROR')) %>%
  select(AU_ID, ben_use, status) %>%
  mutate(type = case_when(grepl("LK", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("EB", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("SR", AU_ID) ~ 'River/Stream',
                          TRUE ~ "Other"))

wide <- AU_BUs %>%
  group_by(type, status, ben_use) %>%
  summarise(num = n()) %>%
  pivot_wider(names_from = ben_use,
              values_from = num)


write.xlsx(wide, file = "ECOS_stats.xlsx")



# AU layer ----------------------------------------------------------------
library(rgdal)
library(sf)

fgdb <- "C:/Users/tpritch/Downloads/WQ_Assessment_2018_20.gdb (3)/WQ_Assessment_2018_20_FINAL.gdb"


fc_list <- ogrListLayers(fgdb)

watershed <- sf::st_read(fgdb, layer = "AssessmentUnit_OR_Watershed_Area")

watershed <- as.data.frame(watershed)%>%
  select(AU_ID)

rivers_coast <- sf::st_read(fgdb, layer = "AssessmentUnits_OR_Rivers_Coast") %>%
  as.data.frame() %>%
  select(AU_ID)

Waterbodies <-  sf::st_read(fgdb, layer = "AssessmentUnits_OR_Waterbodies") %>%
  as.data.frame() %>%


all_AUs <- bind_rows(watershed,rivers_coast,Waterbodies  ) %>%
  filter(AU_ID != '99') %>%
  distinct() %>%
  mutate(type = case_when(grepl("LK", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("EB", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("SR", AU_ID) ~ 'River/Stream',
                          TRUE ~ "Other"))
  
all_AU_type <- all_AUs %>%
  group_by(type) %>%
  summarise(total = n())


total_AUs <- n_distinct(all_AUs$AU_ID)

unassessed <- AU_BUs %>%
  group_by(type,  ben_use) %>%
  summarise(num = n()) %>%
  left_join(all_AU_type) %>%
  mutate(unassessed = total - num) %>%
  select(-num, -total) %>%
  rename(num = unassessed) %>%
  mutate(status = "Unassessed")

wide <- AU_BUs %>%
  group_by(type, status, ben_use) %>%
  summarise(num = n()) %>%
  bind_rows(unassessed) %>%
  pivot_wider(names_from = ben_use,
              values_from = num) %>%
  arrange(type)



AU_names <- read.csv('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_names.csv',
                     stringsAsFactors = FALSE)

LU_BUs <- read.csv('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU Bus.csv',
                   stringsAsFactors = FALSE) %>%
  select(ben_use_id, ben_use)

AU_with_BUs <- left_join(AU_names, LU_BUs)


################

uses <- read.csv("C:/Users/tpritch/Desktop/uses.csv",
                 stringsAsFactors = FALSE) 

AU_uses <- uses %>%
  select(ASSESSMENT_UNIT_ID, USE_NAME,USE_ATTAINMENT_CODE ) %>%
  mutate(status = case_when(USE_ATTAINMENT_CODE == 'X' ~ 'Unassessed',
                            USE_ATTAINMENT_CODE == 'I' ~ 'Insufficient Data',
                            USE_ATTAINMENT_CODE == 'N' ~ 'Impaired',
                            USE_ATTAINMENT_CODE == 'F' ~ 'Attaining'
                            ),
         type =  case_when(grepl("LK", ASSESSMENT_UNIT_ID) ~ 'Lakes/Reservoirs/Estuary',
                           grepl("EB", ASSESSMENT_UNIT_ID) ~ 'Lakes/Reservoirs/Estuary',
                           grepl("SR", ASSESSMENT_UNIT_ID) ~ 'River/Stream',
                           TRUE ~ "Other") ) 

summarized <- AU_uses %>%
  group_by(type, USE_NAME, status ) %>%
  summarise(num = n()) %>%
  pivot_wider(names_from = USE_NAME,
              values_from = num)

###########################################

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")
LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')


names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

AU_to_ben_use <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_names.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(ben_use_code = as.character(AU_UseCode)) %>%
  select(AU_ID, ben_use_code)
  
  
  
LU_benuses$ben_use_code <- as.character(LU_benuses$ben_use_code)

all_ben_uses <- AU_to_ben_use %>%
  left_join(LU_benuses) %>%
  filter(!is.na(ben_use),
         ben_use != "NULL") %>%
  mutate(ben_use = str_to_title(ben_use))




assessments <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Rollup/ALL BASINS_categories.xlsx")

BU_lookup <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU Bus.csv",
                      stringsAsFactors = FALSE) %>%
  mutate_if(is.integer, as.character)


Assess_BUs <- assessments %>%
  left_join(BU_lookup, by = c("Pollu_ID", "WQstd_code")) %>%
  filter(!is.na(Char_Name))


#Create ordered factor for IR_category 

Assess_BUs_factor <- factor(Assess_BUs$IR_category, levels = c('Unassigned',
                                                               "-",
                                                               "Category 3C",
                                                               "Category 3D",
                                                               "Category 3",
                                                               "Category 3B",
                                                               "Category 2",
                                                               "Category 4A",
                                                               "Category 5"),
                            ordered = TRUE)

Assess_BUs$IR_category <- Assess_BUs_factor


AU_BUs <- Assess_BUs %>%
  group_by(AU_ID, ben_use) %>%
  filter(IR_category == max(IR_category),
         row_number()==1) %>%
  ungroup() %>%
  mutate(status =  case_when(IR_category %in% c("Category 4B","Category 4C","Category 4A", "Category 5","Category 4" ) ~ "Impaired",
                             IR_category %in% c("Category 3C","Category 3D","Category 3B", "Category 3") ~ "Insufficient Data",
                             IR_category %in% c("Category 2","Category 3D","Category 3B") ~ "Attaining",
                             TRUE ~ 'ERROR')) %>%
  select(AU_ID, ben_use, status) %>%
  mutate(type = case_when(grepl("LK", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("EB", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("SR", AU_ID) ~ 'River/Stream',
                          TRUE ~ "Other"))





assessed_AU_BUs <- AU_BUs %>%
  mutate(ben_use = str_to_title(ben_use))


joined <- all_ben_uses %>%
  left_join(assessed_AU_BUs) %>%
  mutate(status = ifelse(is.na(status), "Unassessed", status)) %>%
  mutate(type = case_when(grepl("LK", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("EB", AU_ID) ~ 'Lakes/Reservoirs/Estuary',
                          grepl("SR", AU_ID) ~ 'River/Stream',
                          TRUE ~ "Other"))



unassessed <- joined %>%
  group_by(type,  ben_use, status) %>%
  summarise(num = n()) %>%
  pivot_wider(names_from = ben_use,
              values_from = num)
  


  
  write.xlsx(unassessed, file = "ECOS_stats.xlsx",
             keepNA = TRUE, 
             na.string = '0')
  
