library(tidyverse)

load("ATTAINS/LU_Pollutant.Rdata")


# temperature

temp <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Temperature/Data_Review/Temperature_IR_categorization_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = '132',
         Char_Name = "Temperature",
         WQstd_code = "12",
         Period = "Year Round") %>%
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
  )



# Do
spawn_instant <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Spawning_instantaneous_IR_categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = '154',
         Char_Name = "DO",
         WQstd_code = "3",
         Period = "Spawning")  %>%
  select(AU_ID,
         Period,
         WQstd_code,
         Pollu_ID,
         OWRD_Basin,
         Char_Name,
         IR_category,
         Data_Review_Code,
         Data_Review_Comment,
         Rational
  )

year_instant <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_yearround_instantaneous_IR_categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = '154',
         Char_Name = "DO",
         WQstd_code = "3",
         Period = "Year Round") %>%
  select(AU_ID,
         Period,
         OWRD_Basin,
         WQstd_code,
         Pollu_ID, 
         IR_category,
         Data_Review_Code,
         Data_Review_Comment,
         Rational,
         Char_Name
  )


# Ecoli
fresh_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Fresh_Contact_IR_Categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = '76',
         Char_Name = "E coli",
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
  )




# chl

chl_a <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/chl_a/Data_Review/Chla_IR_categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
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
         
  )




#biocrit

#pH
ph <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/Goose & Summer Lakes/pH_IR_categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
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
  )




#tox al - ammonia
tox_al_ammonia <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Ammonia_IR_Categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = '6',
         WQstd_code = "15") %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         OWRD_Basin,
         IR_category,
         Data_Review_Code
         
  )



#toxal-copper
# all file
tox_al_copper <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_CU_BLM_IR_Categories_ALLDATA.csv", stringsAsFactors = FALSE) %>%
  filter(OWRD_Basin == "Goose & Summer Lakes") %>%
  mutate(Pollu_ID = '45',
         Char_Name = "Copper",
         WQstd_code = "15") %>%
  select(AU_ID,
         WQstd_code,
         Char_Name,
         Pollu_ID, 
         OWRD_Basin,
         IR_category,
         Data_Review_Code)
  


#toxal - hardness

tox_al_hardness <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Hardness_Metals_IR_Categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = "",
         WQstd_code = "15") %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         OWRD_Basin,
         IR_category,
         Data_Review_Code
         #Data_Review_Comment,
         #Rational
  )
  


#toxal - penta

toxal_penta <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Pentachlorophenol_IR_categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
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
  )


#toxal - others
toxal <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Others_IR_Categories_Goose & Summer Lake_V1.csv", stringsAsFactors = FALSE) %>%
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
  )


#toxics HH
tox_hh <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_HH/Data_Review/Tox_HH_IR_Categories_Goose & Summer Lakes.csv", stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = "16") %>%
  select(AU_ID,
         Char_Name,
         WQstd_code,
         OWRD_Basin,
         Pollu_ID,
         IR_category,
         Data_Review_Code,
         Data_Review_Comment,
         Rational
  )





#put it together

put_together_initial <- bind_rows(temp,spawn_instant, year_instant,  fresh_contact,
                          chl_a, ph, tox_al_ammonia,  tox_al_copper,  tox_al_hardness,
                          toxal_penta, toxal, tox_hh) %>%
  filter(Char_Name != "Endrin + cis-Nonachlor")

#Pollu_IDs[157,2] = 'Azinphos-methyl'
#Pollu_IDs[157,1] = '89'
# names(Pollu_IDs) <- c('LU_Pollu_ID', 'LU_Pollutant')
# Pollu_IDs <- Pollu_IDs %>%
#   mutate(LU_Pollutant = str_trim(LU_Pollutant, side = c("both", "left", "right")))
# 
# save(Pollu_IDs, file="ATTAINS/LU_Pollutant.Rdata")
# 

#Get remaining Pollu_IDs 

put_together <- put_together_initial %>%
  mutate(Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
                               Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
                               Char_Name == 'DDT' ~ "DDT 4,4'",
                               Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
                               TRUE ~ Char_Name)) %>%
  left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
  mutate(Pollu_ID = ifelse(is.na(Pollu_ID) | Pollu_ID == "", LU_Pollu_ID, Pollu_ID )) %>%
  select(-LU_Pollu_ID) %>%
  arrange(AU_ID)

write.csv(put_together, "ATTAINS/Rollup/Basin_categories/Goose & Summer Lakes_categories.csv",
          row.names = FALSE,
          na = "")


listings_2012 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/ATTAINS_uploads/ATTAINS_download/parameter_12june19 polluIDcopy.csv",
                          stringsAsFactors = FALSE) %>%
  select(ASSESSMENT_UNIT_ID, Pollu_ID, PARAM_ATTAINMENT_CODE) %>%
  rename(AU_ID = ASSESSMENT_UNIT_ID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID))

crosswalked <- put_together %>%
  left_join(listings_2012)

write.csv(crosswalked, "ATTAINS/Rollup/Basin_categories/Goose & Summer Lakes_crosswalk.csv",
          row.names = FALSE,
          na = "")












library(tidyverse)


# Setup -------------------------------------------------------------------

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



load("ATTAINS/LU_Pollutant.Rdata")


for (i in 1:length(Basins)){
  
  basin <- Basins[i]
  
  
  print(paste("Starting basin:",basin ))
  
  # Temperature -------------------------------------------------------------
  
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Temperature_IR_categorization_',basin, '.csv'))) {
    
    temp <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                            basin,
                            "/",
                            'Temperature_IR_categorization_',basin, '.csv'), stringsAsFactors = FALSE) %>%
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
      )
  }
  
  
  # Bacteria ----------------------------------------------------------------
  
  
  # fresh_contact -------------------------------------------------------------------
  
  
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                         basin,
                         "/",
                         'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'))) {
    
    fresh_contact <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                     basin,
                                     "/",
                                     'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = '76',
             Char_Name = "E coli",
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
      )
  }
  
  
  
  # Coast contact -----------------------------------------------------------
  
  
  
  
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
      rename(Data_Review_Comment = X) %>%
      select(AU_ID,
             Char_Name,
             Pollu_ID, 
             WQstd_code,
             OWRD_Basin,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      )
    
    
  }
  
  
  
  # Shellfish harvesting ----------------------------------------------------
  
  
  
  
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
      )
  }
  
  
  
  # Chl ---------------------------------------------------------------------
  
  
  
  
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
             
      )
  }
  
  
  
  # pH ----------------------------------------------------------------------
  
  
  
  
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
      )
    
  }
  
  
  
  # Tox AL ------------------------------------------------------------------
  
  
  # Tox_al_ammonia ----------------------------------------------------------
  
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
             
      )
  }
  
  
  
  
  # tox AL - copper ---------------------------------------------------------
  # all file
  
  
  
  
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
           Data_Review_Comment)
  
  
  
  # hardness ----------------------------------------------------------------
  
  
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
      )
    
  }
  
  
  
  # TOX AL penta ------------------------------------------------------------
  
  
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
      )
  }
  
  
  # TOX AL others -----------------------------------------------------------
  
  
  
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
      )
    
    
  }
  
  
  
  # TOX HH ------------------------------------------------------------------
  
  
  
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
      select(AU_ID,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      )
    
  }
  
  
  
  
  # put it all together -----------------------------------------------------
  
  
  put_together_initial <- bind_rows(temp,  fresh_contact,
                                    chl_a, ph, tox_al_ammonia,  tox_al_copper,  tox_al_hardness,
                                    toxal_penta, toxal, tox_hh) %>%
    filter(Char_Name != "Endrin + cis-Nonachlor")
  
  
  put_together <- put_together_initial %>%
    mutate(Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
                                 Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
                                 Char_Name == 'DDT' ~ "DDT 4,4'",
                                 Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
                                 TRUE ~ Char_Name)) %>%
    left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
    mutate(Pollu_ID = ifelse(is.na(Pollu_ID) | Pollu_ID == "", LU_Pollu_ID, Pollu_ID )) %>%
    select(-LU_Pollu_ID) %>%
    arrange(AU_ID)
  
  
  
  write.csv(put_together, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_categories.csv",
                                 row.names = FALSE,
                                 na = ""))
  
  
  # End ---------------------------------------------------------------------
  
  
}












