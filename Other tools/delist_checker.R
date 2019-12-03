# Delisting checker

library(tidyverse)
library(openxlsx)

# List of basins to use to bring data in

Basins <- c(
  'Columbia River',
  'Deschutes',
  'Goose & Summer Lakes',
  'Grande Ronde',
  'Hood',
  'John Day',
  'Klamath',
  'Malheur',
  'Malheur Lake',
  'Mid Coast',
  'North Coast',
  'Owyhee',
  'Powder',
  'Rogue',
  'Sandy',
  'South Coast',
  'Umatilla',
  'Umpqua',
  'Willamette'
)


# Table to assign Pollu_IDs to pollutants
load("ATTAINS/LU_Pollutant.Rdata")
#save(Pollu_IDs, file ="ATTAINS/LU_Poll

#Biocriteria OWRD lookup

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")


OWRD_query <- glue::glue_sql("SELECT DISTINCT [AU_ID]
      ,[OWRD_Basin]
     
  FROM [IntegratedReport].[dbo].[BioCriteria]", .con = con)

OWRD_lookup <- DBI::dbGetQuery(con, OWRD_query) %>%
  group_by(AU_ID) %>%
  summarise(OWRD_Basin = first(OWRD_Basin))

Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right"))


DBI::dbDisconnect(con)


put_together_list <- list()


# loop through each basin folder and get the assessed data. Write some preliminary tables

for (i in 1:length(Basins)) {
  
  
  basin <- Basins[i]
  
  
  
  print(paste("Starting basin:",basin ))
  
  
  # The general format of all the parameter tables is the same.
  # If object already exists in environment, remove it
  # If assessment file exists, load it in and do some general formatting
  # needed to combine everythinng together.
  # Temperature -------------------------------------------------------------
  
  print("Starting Temperature")
  
  if(exists('temp')){
    rm(temp)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Temperature_IR_categorization_',basin, '.csv'))) {
    
    temp <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                            basin,
                            "/",
                            'Temperature_IR_categorization_',basin,  '_with_validation_error_fix.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '132',
             Char_Name = "Temperature",
             WQstd_code = "12") %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID,
             WQstd_code,
             Period,
             OWRD_Basin,
             IR_category,
             analysis_comment,
             Data_Review_Code,
             Data_Review_Comment,
             Rational
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment)) %>%
      filter(!is.na(AU_ID))
  }
  
  
  # Bacteria ----------------------------------------------------------------
  
  
  # fresh_contact -------------------------------------------------------------------
  
  
  print("Starting fresh_contact")
  
  
  if(exists('fresh_contact')){
    rm(fresh_contact)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'))) {
    
    fresh_contact <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                     basin,
                                     "/",
                                     'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '76',
             Char_Name = "E. coli",
             WQstd_code = "1") %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment,
             Rational
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  }
  
  
  
  # Coast contact -----------------------------------------------------------
  
  print("Starting coast contact")
  
  
  
  if(exists('coast_contact')){
    rm(coast_contact)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Bacteria_Coast_Contact_IR_Categories_',basin, '.csv'))) {
    
    coast_contact <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                     basin,
                                     "/",
                                     'Bacteria_Coast_Contact_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '83',
             Char_Name = "Enterococcus",
             WQstd_code = '1') %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
    
  }
  
  
  
  # Shellfish harvesting ----------------------------------------------------
  
  
  print("Starting shellfish harvest")
  
  if(exists('shell_harvesting')){
    rm(shell_harvesting)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Bacteria_Shell_Harvest_IR_Categories_',basin, '.csv'))) {
    
    shell_harvesting <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                        basin,
                                        "/",
                                        'Bacteria_Shell_Harvest_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '86',
             Char_Name = "Fecal Coliform",
             WQstd_code = '1') %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  }
  
  
  
  # Chl ---------------------------------------------------------------------
  
  
  print("Starting chl")
  
  if(exists('chl')){
    rm(chl)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Chla_IR_categories_',basin, '.csv'))) {
    
    chl <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                           basin,
                           "/",
                           'Chla_IR_categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '40',
             Char_Name = "Chl",
             WQstd_code = "17") %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code, 
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
             
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  }
  
  
  
  # pH ----------------------------------------------------------------------
  
  print("Starting pH")
  
  if(exists('pH')){
    rm(pH)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'pH_IR_categories_',basin, '.csv'))) {
    
    pH <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                          basin,
                          "/",
                          'pH_IR_categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '124',
             Char_Name = "pH",
             WQstd_code = "10") %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment,
             Rational
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  
  
  # Tox AL ------------------------------------------------------------------
  
  
  # Tox_al_ammonia ----------------------------------------------------------
  
  
  print("Starting tox AL Ammonia")
  if(exists('tox_al_ammonia')){
    rm(tox_al_ammonia)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'TOX_AL_Ammonia_IR_Categories_',basin, '.csv'))) {
    
    
    
    #tox al - ammonia
    tox_al_ammonia <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                      basin,
                                      "/",
                                      'TOX_AL_Ammonia_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '6',
             WQstd_code = "15") %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
             
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  }
  
  
  
  
  # tox AL - copper ---------------------------------------------------------
  # all file
  
  
  if(exists('tox_al_copper')){
    rm(tox_al_copper)
  }
  print("Starting tox AL copper")
  tox_al_copper <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_CU_BLM_IR_Categories_ALLDATA.csv", stringsAsFactors = FALSE) %>%
    filter(OWRD_Basin == basin) %>%
    mutate(Pollu_ID = '45',
           Char_Name = "Copper",
           WQstd_code = "15") %>%
    select(AU_ID,
           WQstd_code,
           Char_Name,
           Pollu_ID, 
           OWRD_Basin,
           IR_category,
           Data_Review_Code,
           Data_Review_Comment) %>%
    mutate(Data_Review_Code = as.character(Data_Review_Code),
           Data_Review_Comment = as.character(Data_Review_Comment))
  
  
  
  #biocriteria
  print("Starting biocriteria")
  
  
  
  
  # Biocriteria -------------------------------------------------------------
  
  
  if(exists('biocriteria')){
    rm(biocriteria)
  }
  biocriteria <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Biocriteria/MWCF_Proposed_withnotes_CSV.csv",
                          stringsAsFactors = FALSE)
  
  biocriteria_joined <- biocriteria %>%
    left_join(OWRD_lookup) %>%
    mutate(OWRD_Basin = case_when(AU_ID == 'OR_SR_1710030306_02_105179' ~ "Umpqua",
                                  AU_ID == 'OR_SR_1710030601_02_104997' ~ "South Coast",
                                  AU_ID == 'OR_WS_171003040106_02_105316' ~ "South Coast",
                                  AU_ID == 'OR_WS_171003040301_02_105016' ~ "South Coast",
                                  TRUE ~ OWRD_Basin)) %>%
    rename(Data_Review_Comment = Combined.notes,
           IR_category = IR_Category_2018) %>%
    mutate(Char_Name = "Biocriteria",
           Pollu_ID = '156',
           WQstd_code = '5') %>%
    select(AU_ID,
           Char_Name,
           WQstd_code,
           OWRD_Basin,
           Pollu_ID,
           IR_category,
           Data_Review_Comment
    ) %>%
    filter(OWRD_Basin == basin)
  
  
  
  # narrative ---------------------------------------------------------------
  
  print("Starting narrative")
  if(exists('narrative')){
    rm(narrative)
  }
  narrative <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Narrative Standard Assessment/narrative assessment put together.csv",
                        stringsAsFactors = FALSE) %>%
    filter(AU_ID != "") %>%
    filter(OWRD_Basin == basin) %>%
    mutate(Pollu_ID = as.character(Pollu_ID),
           WQstd_code = as.character(WQstd_code))
  
  
  # hardness ----------------------------------------------------------------
  
  print("Starting tox AL hardness")
  
  if(exists('tox_al_hardness')){
    rm(tox_al_hardness)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'TOX_AL_Hardness_Metals_IR_Categories_',basin, '.csv'))) {
    
    #toxal - hardness
    
    tox_al_hardness <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                       basin,
                                       "/",
                                       'TOX_AL_Hardness_Metals_IR_Categories_',basin, '.csv'), 
                                stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "",
             WQstd_code = "15") %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment,
             #Rational
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment)) %>%
      left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
      mutate(Pollu_ID = LU_Pollu_ID) %>%
      select(-LU_Pollu_ID)
    
  }
  
  
  
  # TOX AL penta ------------------------------------------------------------
  
  
  
  print("Starting tox AL Penta")
  
  
  if(exists('toxal_penta')){
    rm(toxal_penta)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'TOX_AL_Pentachlorophenol_IR_categories_',basin, '.csv'))) {
    
    toxal_penta <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                   basin,
                                   "/",
                                   'TOX_AL_Pentachlorophenol_IR_categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '123',
             Char_Name = "Pentachlorophenol",
             WQstd_code = "15") %>%
      rename(IR_category = Category) %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             OWRD_Basin,
             WQstd_code,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment,
             Rational
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  }
  
  
  # TOX AL others -----------------------------------------------------------
  
  
  print("Starting tox AL others")
  
  if(exists('toxal')){
    rm(toxal)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'TOX_AL_Others_IR_Categories_',basin, '.csv'))) {
    
    
    #toxal - others
    toxal <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                             basin,
                             "/",
                             'TOX_AL_Others_IR_Categories_',basin, '.csv'), 
                      stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "",
             WQstd_code = "15") %>%
      select(AU_ID,
             OWRD_Basin,
             Char_Name,
             WQstd_code,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment,
             Rational
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment)) %>%
      left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
      mutate(Pollu_ID = LU_Pollu_ID) %>%
      select(-LU_Pollu_ID) %>%
      mutate(Pollu_ID = case_when(Char_Name == 'Alkalinity, total' ~ '5', 
                                  Char_Name == 'PCBs' ~ '153',
                                  Char_Name == 'DDT' ~ '50',
                                  Char_Name == "Lindane" ~ '22',
                                  TRUE ~ Pollu_ID )) %>%
      filter(Char_Name != 	'Endrin + cis-Nonachlor')
    
    
  }
  
  
  
  # TOX HH ------------------------------------------------------------------
  
  print("Starting tox AL HH")
  
  if(exists('tox_hh')){
    rm(tox_hh)
  }
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Tox_HH_IR_Categories_',basin, '.csv'))) {
    
    
    
    
    tox_hh <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                              basin,
                              "/",
                              'Tox_HH_IR_Categories_',basin, '.csv'), 
                       stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = as.character(Pollu_ID),
             WQstd_code = "16") %>%
      group_by(AU_ID, Char_Name) %>%
      mutate(match = ifelse(Simplified_sample_fraction == Crit_Fraction, 1, 0 ),
             keep = ifelse(max(match, na.rm = TRUE) == 1 & match == 1 |
                             max(match, na.rm = TRUE) == 0 & match == 0, 1, 0)) %>%
      filter(keep == 1) %>%
      select(AU_ID,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment)) %>%
      distinct()
    
  }
  
  
  # tissue mercury ----------------------------------------------------------
  
  if(exists('tox_hh_hg_tissue')){
    rm(tox_hh_hg_tissue)
  }
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Tox_HH_hg_tissue_IR_Categories_',basin, '.csv'))) {
    
    tox_hh_hg_tissue <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                        basin,
                                        "/",
                                        'Tox_HH_hg_tissue_IR_Categories_',basin, '.csv'), 
                                 stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = as.character(Pollu_ID),
             WQstd_code = "16") %>%
      select(AU_ID,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  # Dissolved Oxyegn --------------------------------------------------------
  
  
  if(exists('DO_yrround_cont')){
    rm(DO_yrround_cont)
  }
  
  print("Starting DO yearround continuous")
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'DO_yearround_continuous_IR_categories_',basin, '.csv'))) {
    
    DO_yrround_cont <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                       basin,
                                       "/",
                                       'DO_yearround_continuous_IR_categories_',basin, '.csv'),
                                stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Year Round") %>%
      group_by(AU_ID) %>%
      mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
      ungroup() %>%
      mutate(assess_type = ifelse(mult_flag == 1, DO_Class, NA )) %>%
      select(AU_ID,
             Period,
             Char_Name,
             assess_type,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  print("Starting DO yearround instant")
  
  if(exists('DO_yrround_inst')){
    rm(DO_yrround_inst)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'DO_yearround_instantaneous_IR_categories_',basin, '.csv'))) {
    
    DO_yrround_inst <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                       basin,
                                       "/",
                                       'DO_yearround_instantaneous_IR_categories_',basin, '.csv'),
                                stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Year Round") %>%
      group_by(AU_ID) %>%
      mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
      ungroup() %>%
      mutate(assess_type = ifelse(mult_flag == 1, DO_Class, NA )) %>%
      select(AU_ID,
             Period,
             Char_Name,
             assess_type,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  print("Starting DO spawn continuous")
  
  if(exists('DO_spawn_cont')){
    rm(DO_spawn_cont)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'DO_Spawning_continuous_IR_categories_',basin, '.csv'))) {
    
    DO_spawn_cont <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                     basin,
                                     "/",
                                     'DO_Spawning_continuous_IR_categories_',basin, '.csv'),
                              stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Spawning") %>%
      group_by(AU_ID) %>%
      # mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
      # ungroup() %>%
      # mutate(assess_type = ifelse(mult_flag == 1, DO_Class, NA )) %>%
      select(AU_ID,
             Period,
             Char_Name,
             # assess_type,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  print("Starting DO spawn instant")
  
  if(exists('DO_spawn_inst')){
    rm(DO_spawn_inst)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'))) {
    
    DO_spawn_inst <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                     basin,
                                     "/",
                                     'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'),
                              stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Spawning") %>%
      group_by(AU_ID) %>%
      # mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
      # ungroup() %>%
      # mutate(assess_type = ifelse(mult_flag == 1, DO_Class, NA )) %>%
      select(AU_ID,
             Period,
             Char_Name,
             #assess_type,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  
  
  if(exists('DO_spawn_estuary')){
    rm(DO_spawn_estuary)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'DO_Estuary_Spawn_IR_categories_',basin, '.csv'))) {
    
    DO_spawn_estuary <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                        basin,
                                        "/",
                                        'DO_Estuary_Spawn_IR_categories_', basin, '.csv'),
                                 stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Spawning") %>%
      group_by(AU_ID) %>%
      mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
      ungroup() %>%
      mutate(assess_type = "Estuary") %>%
      select(AU_ID,
             Period,
             Char_Name,
             assess_type,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  
  if(exists('DO_year_estuary')){
    rm(DO_year_estuary)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'DO_Estuary_Yearround_IR_categories_',basin, '.csv'))) {
    
    DO_year_estuary <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                       basin,
                                       "/",
                                       'DO_Estuary_Yearround_IR_categories_', basin, '.csv'),
                                stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Year Round") %>%
      group_by(AU_ID) %>%
      mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
      ungroup() %>%
      mutate(assess_type = "Estuary") %>%
      select(AU_ID,
             Period,
             Char_Name,
             assess_type,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  
  print("Starting shellfish")
  if(exists('shellfish_toxins')){
    rm(shellfish_toxins)
  }
  shellfish_toxins <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/shellfish_categories.xlsx") %>%
    filter(AU_ID != "") %>%
    filter(OWRD_Basin == basin) %>%
    mutate(Pollu_ID = as.character(Pollu_ID),
           WQstd_code = as.character(WQstd_code)) %>%
    rename(analysis_comment = analysis_comment_2018) %>%
    select(AU_ID,
           Period,
           Char_Name,
           WQstd_code,
           analysis_comment,
           OWRD_Basin,
           Pollu_ID,
           IR_category,
           Data_Review_Code,
           Data_Review_Comment
    ) 
  
  
  
  
  # OA ----------------------------------------------------------------------
  ocean_listings <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/OA_categories.xlsx") %>%
    filter(AU_ID != "") %>%
    filter(OWRD_Basin == basin) %>%
    mutate(Pollu_ID = as.character(Pollu_ID),
           WQstd_code = as.character(WQstd_code)) %>%
    rename(analysis_comment = analysis_comment_2018) %>%
    select(AU_ID,
           Period,
           Char_Name,
           analysis_comment,
           WQstd_code,
           OWRD_Basin,
           Pollu_ID,
           IR_category,
           Data_Review_Code,
           Data_Review_Comment
    ) 
  
  # put it all together -----------------------------------------------------
  
  put_together_initial <- bind_rows(get0('temp'),  get0('fresh_contact'),
                                    get0('coast_contact'), get0('shell_harvesting'),
                                    get0('chl'), get0('pH'), get0('tox_al_ammonia'), 
                                    get0('tox_al_copper'), get0('tox_al_hardness'), 
                                    get0('toxal_penta'), get0('toxal'), 
                                    get0('tox_hh'), get0('DO_spawn_inst') ,get0('DO_spawn_cont'),
                                    get0('DO_yrround_inst'), get0('DO_yrround_cont'), 
                                    get0('biocriteria_joined'), get0('narrative'), 
                                    get0('tox_hh_hg_tissue'),
                                    get0('DO_year_estuary'),
                                    get0('DO_spawn_estuary'),
                                    get0('shellfish_toxins') ,
                                    get0('ocean_listings')) 
  
  
  
  put_together_list[[i]] <- put_together_initial
  
}

all_2018_categories <-bind_rows(put_together_list)  %>%
  filter(AU_ID != "") %>%
  mutate(IR_category = case_when(grepl("5", IR_category) ~ "Category 5",
                                 grepl("2", IR_category) ~ "Category 2",
                                 grepl("3B", IR_category) ~ "Category 3B",
                                 grepl("3D", IR_category) ~ "Category 3D",
                                 grepl("3C", IR_category) ~ "Category 3C",
                                 grepl("3", IR_category) ~ "Category 3",
                                 IR_category == '-' ~ '-',
                                 IR_category == "Unassigned" ~ "Unassigned",
                                 TRUE ~ "Error"),
         Period = case_when(Period %in% c("Year_Round", "Year round") ~ "Year Round",
                            TRUE ~ Period))


Cat_2_2018 <- all_2018_categories %>%
  filter(IR_category == "Category 2")

# bring in 2012 list


previous_listings <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/ATTAINS_uploads/ATTAINS_download/2012Crosswalk_Final.csv",
                              stringsAsFactors = FALSE) %>%
  select(ASSESSMENT_UNIT_ID, Pollu_ID, PARAM_ATTAINMENT_CODE,WQstrd_code, PARAM_YEAR_LISTED, PARAM_NAME, Time_Period) %>%
  rename(Period = Time_Period) %>%
  distinct() %>%
  mutate(previous_IR_category = "Category 5",
         Pollu_ID = as.character(Pollu_ID),
         Period = case_when(Period == "Year_Round" ~ "Year Round",
                            TRUE ~ Period))%>%
  left_join(select(Pollutants, Pollu_ID, Pollutant_DEQ.WQS), by = "Pollu_ID") %>%
  mutate(PARAM_NAME = Pollutant_DEQ.WQS) %>%
  select(-Pollutant_DEQ.WQS) %>%
  rename(AU_ID = ASSESSMENT_UNIT_ID,
         WQstd_code = WQstrd_code) %>%
  mutate(WQstd_code = as.character(WQstd_code)) %>%
  group_by(AU_ID, Pollu_ID, WQstd_code) %>%
  filter(PARAM_YEAR_LISTED == min(PARAM_YEAR_LISTED, na.rm = TRUE)) %>%
  mutate(Period = ifelse(Period == "", NA, Period)) %>%
  select(-PARAM_ATTAINMENT_CODE)

delistings <- Cat_2_2018 %>%
  left_join(previous_listings, by = c("AU_ID", "Pollu_ID", "WQstd_code", "Period")) %>%
  filter(previous_IR_category == "Category 5")




# Compare to reviewd delist list ------------------------------------------


reviewed_delistings <- read.xlsx('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Communications/Public Comment/Allbasins_delisting_V8_forEPA.xlsx') %>%
  select(AU_ID, Pollu_ID,WQstd_code,Period, Category_Final_Proposed, Data_Review_Comment, Rationale) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = as.character(WQstd_code)) %>%
  mutate(Period = ifelse(Period == 'Year round', 'Year Round', Period ))
 

comparison <- delistings %>%
  full_join(reviewed_delistings, by = c("AU_ID", "Pollu_ID", "WQstd_code", "Period"))




write.xlsx(comparison, 'Other tools/delist_check.xlsx')

  