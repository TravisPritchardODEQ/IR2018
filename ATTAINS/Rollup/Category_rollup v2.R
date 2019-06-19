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



#save(Pollu_IDs, file = "ATTAINS/LU_Pollutant.Rdata")

load("ATTAINS/LU_Pollutant.Rdata")


BUs <- read.csv("ATTAINS/LU Bus.csv") %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = as.character(WQstd_code))


AU_to_ben_use <- read.csv("ATTAINS/AU_to_ben_use.csv",
                          stringsAsFactors = FALSE)

names(AU_to_ben_use) <- c("AU_ID", "ben_use_code")


LU_benuses <- read.csv("ATTAINS/LU_ben_uses.csv", stringsAsFactors = FALSE)

names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

LU_benuses$ben_use_code <- as.character(LU_benuses$ben_use_code)


all_ben_uses <- AU_to_ben_use %>%
  left_join(LU_benuses) %>%
  filter(!is.na(ben_use),
         ben_use != "NULL")


#Biocriteria OWRD lookup

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")


OWRD_query <- glue::glue_sql("SELECT DISTINCT [AU_ID]
      ,[OWRD_Basin]
     
  FROM [IntegratedReport].[dbo].[BioCriteria]", .con = con)

OWRD_lookup <- DBI::dbGetQuery(con, OWRD_query) %>%
  group_by(AU_ID) %>%
  summarise(OWRD_Basin = first(OWRD_Basin))


DBI::dbDisconnect(con)



put_together_list <- list()
delist_list <- list()
BU_rollup_list <- list()
BU_counts_list <- list()


for (i in 1:length(Basins)){
  
  basin <- Basins[i]

  
  print(paste("Starting basin:",basin ))

# Temperature -------------------------------------------------------------

print("Starting Temperature")

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
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code)) %>%
      filter(is.na(AU_ID))
  }
  

# Bacteria ----------------------------------------------------------------


# fresh_contact -------------------------------------------------------------------

  
  print("Starting fresh_contact")
  
  
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
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code))
  }
  
  

# Coast contact -----------------------------------------------------------
  
  print("Starting coast contact")
  
  
  
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
      mutate(Data_Review_Code = as.character(Data_Review_Code))
  
  
  }
  
  

# Shellfish harvesting ----------------------------------------------------

  
  print("Starting shellfish harvest")
  
  
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
      mutate(Data_Review_Code = as.character(Data_Review_Code))
}
  
  

# Chl ---------------------------------------------------------------------

  
  print("Starting chl")
  
  
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
      mutate(Data_Review_Code = as.character(Data_Review_Code))
  }
  
  

# pH ----------------------------------------------------------------------

  print("Starting pH")
  
  
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
      mutate(Data_Review_Code = as.character(Data_Review_Code))
    
  }
  
  

# Tox AL ------------------------------------------------------------------


# Tox_al_ammonia ----------------------------------------------------------

  
  print("Starting tox AL Ammonia")
  
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
      mutate(Data_Review_Code = as.character(Data_Review_Code))
  }
  
  
  

# tox AL - copper ---------------------------------------------------------
  # all file
  
  
  
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
    mutate(Data_Review_Code = as.character(Data_Review_Code))
  
  
  
  #biocriteria
  print("Starting biocriteria")
  

  

# Biocriteria -------------------------------------------------------------

  
  
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

   
  #biocriteria
  print("Starting narrative")
  
  narrative <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Narrative Standard Assessment/narrative assessment put together.csv",
                        stringsAsFactors = FALSE) %>%
    filter(AU_ID != "") %>%
    filter(OWRD_Basin == basin) %>%
    mutate(Pollu_ID = as.character(Pollu_ID),
           WQstd_code = as.character(WQstd_code))
  

# hardness ----------------------------------------------------------------

  print("Starting tox AL hardness")
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
    mutate(Data_Review_Code = as.character(Data_Review_Code))
  
  }
  
  

# TOX AL penta ------------------------------------------------------------
  
  print("Starting tox AL Penta")
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
    mutate(Data_Review_Code = as.character(Data_Review_Code))
  }
  

# TOX AL others -----------------------------------------------------------

  
  print("Starting tox AL others")
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
      mutate(Data_Review_Code = as.character(Data_Review_Code))
  
  
  }
  
  

# TOX HH ------------------------------------------------------------------

  print("Starting tox AL HH")
  
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
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code))
    
  }
  
  

# Dissolved Oxyegn --------------------------------------------------------


  
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
      select(AU_ID,
             Period,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code))
    
  }
  
  print("Starting DO yearround instant")
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                          basin,
                          "/",
                          'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'))) {
    
    DO_yrround_inst <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                       basin,
                                       "/",
                                       'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'),
                                stringsAsFactors = FALSE) %>%
      mutate(Pollu_ID = "154",
             WQstd_code = "3",
             Char_Name = "Dissolved Oxygen",
             Period = "Year Round") %>%
      select(AU_ID,
             Period,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code))
  
  }
  
  print("Starting DO spawn continuous")
  
  
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
      select(AU_ID,
             Period,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code))
    
  }
  
  print("Starting DO spawn instant")
  
  
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
      select(AU_ID,
             Period,
             Char_Name,
             WQstd_code,
             OWRD_Basin,
             Pollu_ID,
             IR_category,
             Data_Review_Code,
             Data_Review_Comment
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code))
    
  }
  
  
  
# put it all together -----------------------------------------------------
print('Writing tables')
  
  put_together_initial <- bind_rows(get0('temp'),  get0('fresh_contact'),
                                    get0('coast_contact'), get0('shell_harvesting'),
                                    get0('chl'), get0('pH'), get0('tox_al_ammonia'), 
                                    get0('tox_al_copper'), get0('tox_al_hardness'), 
                                    get0('toxal_penta'), get0('toxal'), 
                                    get0('tox_hh'), get0('DO_spawn_inst') ,get0('DO_spawn_cont'),
                                    get0('DO_yrround_inst'), get0('DO_yrround_cont'), 
                                    get0('biocriteria_joined'), get0('narrative')) %>%
    filter(Char_Name != "Endrin + cis-Nonachlor") %>%
    mutate(IR_category = case_when(grepl("5", IR_category) ~ "Category 5",
                                   grepl("2", IR_category) ~ "Category 2",
                                   grepl("3B", IR_category) ~ "Category 3B",
                                   grepl("3D", IR_category) ~ "Category 3D",
                                   grepl("3C", IR_category) ~ "Category 3C",
                                   grepl("3", IR_category) ~ "Category 3",
                                   IR_category == '-' ~ '-',
                                   TRUE ~ "Error")) 
  
  IR_category_factor <- factor(put_together_initial$IR_category, levels = c('Unassigned',
                                                                            "-",
                                                                            "Category 3C",
                                                                            "Category 3D",
                                                                            "Category 3",
                                                                            "Category 3B",
                                                                            "Category 2",
                                                                            "Category 5"),
                               ordered = TRUE)
  
  put_together_initial$IR_category <- IR_category_factor
  
  
  put_together <- put_together_initial %>%
    mutate(Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
                                 Char_Name == "Alkalinity, bicarbonate" ~ 'Alkalinity',
                                 Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
                                 Char_Name == 'DDT' ~ "DDT 4,4'",
                                 Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
                                 TRUE ~ Char_Name)) %>%
    left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
    mutate(Pollu_ID = ifelse(is.na(Pollu_ID) | Pollu_ID == "", LU_Pollu_ID, Pollu_ID )) %>%
    select(-LU_Pollu_ID) %>%
    arrange(AU_ID)
  
  
  
  write.csv(put_together, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_categories.csv"),
            row.names = FALSE,
            na = "")
  
  
  listings_2012 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/ATTAINS_uploads/ATTAINS_download/parameter_12june19 polluIDcopy.csv",
                            stringsAsFactors = FALSE) %>%
    select(ASSESSMENT_UNIT_ID, Pollu_ID, PARAM_ATTAINMENT_CODE) %>%
    rename(AU_ID = ASSESSMENT_UNIT_ID) %>%
    mutate(Pollu_ID = as.character(Pollu_ID)) %>%
    distinct()
  
  crosswalked <- put_together %>%
    left_join(listings_2012) %>%
    filter(grepl('2', IR_category),
           !is.na(PARAM_ATTAINMENT_CODE)) %>%
    rename(Previous_PARAM_ATTAINMENT_CODE = PARAM_ATTAINMENT_CODE)
  
  
  write.csv(crosswalked, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_delistings.csv"),
                                 row.names = FALSE,
                                 na = "")
  
  
  
  BU_s <- put_together %>%
    left_join(BUs, by = c("Pollu_ID", "WQstd_code") )
  
  # 
  # BU_rollup_setup <- BU_s %>%
  #   mutate(IR_category = as.factor(IR_category))
  # 
  # BU_rollup_setup$IR_category <- factor(BU_rollup_setup$IR_category,levels(BU_rollup_setup$IR_category)[c(1,3,2,4)], ordered = TRUE)
  # 

 
  # 
  # BU_rollup <- BU_s %>%
  #   group_by(AU_ID, ben_use) %>%
  #   summarise(Category = max(IR_category)) %>%
  #   spread(ben_use, Category, fill = "-")
  
  # write.csv(BU_rollup, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_BU_rollup.csv"),
  #           row.names = FALSE,
  #           na = "")
  
  BU_counts <- BU_s %>%
    group_by(AU_ID, ben_use, IR_category) %>%
    summarise(num_cats = n()) %>%
    spread(IR_category, num_cats, fill = "-") 
  
  
  
  write.csv(BU_counts, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_BU_counts.csv"),
                                row.names = FALSE,
                                na = "")
  
  
  put_together_list[[i]] <- put_together
  delist_list[[i]] <- crosswalked
  BU_rollup_list[[i]] <- BU_s
  BU_counts_list[[i]] <- BU_counts
# End ---------------------------------------------------------------------

  
  }
    
  
    
 all_categories <-bind_rows(put_together_list)   
 all_delist <- bind_rows(delist_list)  
 all_BU_rollup <- bind_rows(BU_rollup_list) 
 all_BU_counts <- bind_rows(BU_counts_list)
    

 OWRD_basins <- all_categories %>%
   distinct(AU_ID, OWRD_Basin)
 
 
 BU_rollup <- all_BU_rollup %>%
   group_by(AU_ID, ben_use) %>%
   summarise(Category = max(IR_category)) %>%
   right_join(filter(all_ben_uses, AU_ID %in% all_categories$AU_ID)) %>%
     mutate(Category = as.character(Category),
          Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
   select(-ben_use_id) %>%
   #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
   spread(ben_use, Category, fill = "-") %>%
   select(-`Commercial Navigation and Transportation`,
          -`Hydro Power`, -`Industrial Water Supply`, -`Irrigation`,
          -`Livestock Watering`, -`Wildlife and Hunting`)
     
    
    
 write.csv(all_categories, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_categories.csv"),
           row.names = FALSE,
           na = "")

 write.csv(all_delist, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_delistings.csv"),
           row.names = FALSE,
           na = "")
 
 
 
 write.csv(BU_rollup, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_BU_rollup.csv"),
           row.names = FALSE,
           na = "")

 write.csv(all_BU_counts, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_BU_counts.csv"),
           row.names = FALSE,
           na = "")
 