library(tidyverse)
library(openxlsx)

# Assessment Files --------------------------------------------------------



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


#Read in AU adjustments
AU_Fixes <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/AU_Fixes.xlsx") %>%
  select(OLD_AU_ID,NEW_AU_ID )

#Replacement function
Fix_AUs <- function(df){
  
  df <- df %>%
    left_join(AU_Fixes, by = c('AU_ID' = 'OLD_AU_ID')) %>%
    mutate(AU_ID = ifelse(!is.na(NEW_AU_ID), NEW_AU_ID, AU_ID)) %>%
    select(-NEW_AU_ID)
  
  return(df)
}

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
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Temperature_IR_categorization_',basin, '_with_validation_error_fix.csv'))) {
    
    temp <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                            basin,
                            "/",
                            'Temperature_IR_categorization_',basin,  '_with_validation_error_fix.csv'), stringsAsFactors = FALSE) 
    
    
    temp <- Fix_AUs(temp)
    
    write.csv(temp, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                  basin,
                                  "/",
                                  'Temperature_IR_categorization_',basin,  '_with_validation_error_fix.csv'),
              na = "", row.names = FALSE
              )
  }
  
  
  # Bacteria ----------------------------------------------------------------
  
  
  # fresh_contact -------------------------------------------------------------------
  
  
  print("Starting fresh_contact")
  
  
  if(exists('fresh_contact')){
    rm(fresh_contact)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'))) {
    
    fresh_contact <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                     basin,
                                     "/",
                                     'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE)
    
    fresh_contact <- Fix_AUs(fresh_contact)
    
    write.csv(fresh_contact, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                    basin,
                                    "/",
                                    'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  # Coast contact -----------------------------------------------------------
  
  print("Starting coast contact")
  
  
  
  if(exists('coast_contact')){
    rm(coast_contact)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Bacteria_Coast_Contact_IR_Categories_',basin, '.csv'))) {
    
    coast_contact <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                     basin,
                                     "/",
                                     'Bacteria_Coast_Contact_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) 
    coast_contact <- Fix_AUs(coast_contact)
    
    write.csv(coast_contact, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                    basin,
                                    "/",
                                    'Bacteria_Coast_Contact_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  # Shellfish harvesting ----------------------------------------------------
  
  
  print("Starting shellfish harvest")
  
  if(exists('shell_harvesting')){
    rm(shell_harvesting)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Bacteria_Shell_Harvest_IR_Categories_',basin, '.csv'))) {
    
    shell_harvesting <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                        basin,
                                        "/",
                                        'Bacteria_Shell_Harvest_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE)
    shell_harvesting <- Fix_AUs(shell_harvesting)
    
    
    write.csv(shell_harvesting, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                        basin,
                                        "/",
                                        'Bacteria_Shell_Harvest_IR_Categories_',basin, '.csv'),
                                 na = "", 
                                 row.names = FALSE)
    
  }
  
  
  
  # Chl ---------------------------------------------------------------------
  
  
  print("Starting chl")
  
  if(exists('chl')){
    rm(chl)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Chla_IR_categories_',basin, '.csv'))) {
    
    chl <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                           basin,
                           "/",
                           'Chla_IR_categories_',basin, '.csv'), stringsAsFactors = FALSE)
    chl <- Fix_AUs(chl)
    
    write.csv(chl, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                         basin,
                         "/",
                         'Chla_IR_categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  # pH ----------------------------------------------------------------------
  
  print("Starting pH")
  
  if(exists('pH')){
    rm(pH)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'pH_IR_categories_',basin, '.csv'))) {
    
    pH <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                          basin,
                          "/",
                          'pH_IR_categories_',basin, '.csv'), stringsAsFactors = FALSE) 
    pH <- Fix_AUs(pH)
    
    write.csv(pH,
              file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                     basin,
                     "/",
                     'pH_IR_categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  # Tox AL ------------------------------------------------------------------
  
  
  # Tox_al_ammonia ----------------------------------------------------------
  
  
  print("Starting tox AL Ammonia")
  if(exists('tox_al_ammonia')){
    rm(tox_al_ammonia)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Ammonia_IR_Categories_',basin, '.csv'))) {
    
    
    
    #tox al - ammonia
    tox_al_ammonia <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                      basin,
                                      "/",
                                      'TOX_AL_Ammonia_IR_Categories_',basin, '.csv'), stringsAsFactors = FALSE) 
    
    tox_al_ammonia <- Fix_AUs(tox_al_ammonia)
    
    write.csv(tox_al_ammonia, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                     basin,
                                     "/",
                                     'TOX_AL_Ammonia_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
  }
  
  
  
  
  
  # hardness ----------------------------------------------------------------
  
  print("Starting tox AL hardness")
  
  if(exists('tox_al_hardness')){
    rm(tox_al_hardness)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Hardness_Metals_IR_Categories_',basin, '.csv'))) {
    
    #toxal - hardness
    
    tox_al_hardness <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                       basin,
                                       "/",
                                       'TOX_AL_Hardness_Metals_IR_Categories_',basin, '.csv'), 
                                stringsAsFactors = FALSE)
    tox_al_hardness <- Fix_AUs(tox_al_hardness)
    
    write.csv(tox_al_hardness, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                      basin,
                                      "/",
                                      'TOX_AL_Hardness_Metals_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  # TOX AL penta ------------------------------------------------------------
  
  
  
  print("Starting tox AL Penta")
  
  
  if(exists('toxal_penta')){
    rm(toxal_penta)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Pentachlorophenol_IR_categories_',basin, '.csv'))) {
    
    toxal_penta <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                   basin,
                                   "/",
                                   'TOX_AL_Pentachlorophenol_IR_categories_',basin, '.csv'), stringsAsFactors = FALSE) 
    toxal_penta <- Fix_AUs(toxal_penta)
    
    write.csv(toxal_penta,
              file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                            basin,
                            "/",
                            'TOX_AL_Pentachlorophenol_IR_categories_',basin, '.csv'))
    
  }
  
  
  # TOX AL others -----------------------------------------------------------
  
  
  print("Starting tox AL others")
  
  if(exists('toxal')){
    rm(toxal)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Others_IR_Categories_',basin, '.csv'))) {
    
    
    #toxal - others
    toxal <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                             basin,
                             "/",
                             'TOX_AL_Others_IR_Categories_',basin, '.csv'), 
                      stringsAsFactors = FALSE)
    
    
    toxal <- Fix_AUs(toxal)
    
    write.csv(toxal, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                   basin,
                                   "/",
                                   'TOX_AL_Others_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  # TOX HH ------------------------------------------------------------------
  
  print("Starting tox AL HH")
  
  if(exists('tox_hh')){
    rm(tox_hh)
  }
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Tox_HH_IR_Categories_',basin, '.csv'))) {
    
    
    
    
    tox_hh <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                              basin,
                              "/",
                              'Tox_HH_IR_Categories_',basin, '.csv'), 
                       stringsAsFactors = FALSE) 
    
    
    tox_hh <- Fix_AUs(tox_hh)
    
    write.csv(tox_hh, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                    basin,
                                    "/",
                                    'Tox_HH_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  # tissue mercury ----------------------------------------------------------
  
  if(exists('tox_hh_hg_tissue')){
    rm(tox_hh_hg_tissue)
  }
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Tox_HH_hg_tissue_IR_Categories_',basin, '.csv'))) {
    
    tox_hh_hg_tissue <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                        basin,
                                        "/",
                                        'Tox_HH_hg_tissue_IR_Categories_',basin, '.csv'), 
                                 stringsAsFactors = FALSE) 
    
    tox_hh_hg_tissue <- Fix_AUs(tox_hh_hg_tissue)
    
    write.csv(tox_hh_hg_tissue, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                              basin,
                                              "/",
                                              'Tox_HH_hg_tissue_IR_Categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
    
  }
  # Dissolved Oxyegn --------------------------------------------------------
  
  
  if(exists('DO_yrround_cont')){
    rm(DO_yrround_cont)
  }
  
  print("Starting DO yearround continuous")
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'DO_yearround_continuous_IR_categories_',basin, '.csv'))) {
    
    DO_yrround_cont <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                       basin,
                                       "/",
                                       'DO_yearround_continuous_IR_categories_',basin, '.csv'),
                                stringsAsFactors = FALSE) 
    
    DO_yrround_cont <- Fix_AUs(DO_yrround_cont)
    
    write.csv(DO_yrround_cont, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                             basin,
                                             "/",
                                             'DO_yearround_continuous_IR_categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  print("Starting DO yearround instant")
  
  if(exists('DO_yrround_inst')){
    rm(DO_yrround_inst)
  }
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'DO_yearround_instantaneous_IR_categories_',basin, '.csv'))) {
    
    DO_yrround_inst <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                       basin,
                                       "/",
                                       'DO_yearround_instantaneous_IR_categories_',basin, '.csv'),
                                stringsAsFactors = FALSE)
    
    DO_yrround_inst <- Fix_AUs(DO_yrround_inst)
    
    write.csv(DO_yrround_inst, file =paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                            basin,
                                            "/",
                                            'DO_yearround_instantaneous_IR_categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
    
  }
  
  print("Starting DO spawn continuous")
  
  if(exists('DO_spawn_cont')){
    rm(DO_spawn_cont)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'DO_Spawning_continuous_IR_categories_',basin, '.csv'))) {
    
    DO_spawn_cont <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                     basin,
                                     "/",
                                     'DO_Spawning_continuous_IR_categories_',basin, '.csv'),
                              stringsAsFactors = FALSE) 
    
    
    DO_spawn_cont <- Fix_AUs(DO_spawn_cont)
    
    write.csv(DO_spawn_cont, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                           basin,
                                           "/",
                                           'DO_Spawning_continuous_IR_categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  print("Starting DO spawn instant")
  
  if(exists('DO_spawn_inst')){
    rm(DO_spawn_inst)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'))) {
    
    DO_spawn_inst <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                     basin,
                                     "/",
                                     'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'),
                              stringsAsFactors = FALSE) 
    
    DO_spawn_inst <- Fix_AUs(DO_spawn_inst)
    
    write.csv(DO_spawn_inst, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                           basin,
                                           "/",
                                           'DO_Spawning_instantaneous_IR_categories_',basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  
  if(exists('DO_spawn_estuary')){
    rm(DO_spawn_estuary)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'DO_Estuary_Spawn_IR_categories_',basin, '.csv'))) {
    
    DO_spawn_estuary <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                        basin,
                                        "/",
                                        'DO_Estuary_Spawn_IR_categories_', basin, '.csv'),
                                 stringsAsFactors = FALSE) 
    
    DO_spawn_estuary <- Fix_AUs(DO_spawn_estuary)
    
    write.csv(DO_spawn_estuary, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                              basin,
                                              "/",
                                              'DO_Estuary_Spawn_IR_categories_', basin, '.csv'),
              na = "", row.names = FALSE)
    
  }
  
  
  if(exists('DO_year_estuary')){
    rm(DO_year_estuary)
  }
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'DO_Estuary_Yearround_IR_categories_',basin, '.csv'))) {
    
    DO_year_estuary <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                                       basin,
                                       "/",
                                       'DO_Estuary_Yearround_IR_categories_', basin, '.csv'),
                                stringsAsFactors = FALSE) 
    
    DO_year_estuary <- Fix_AUs(DO_year_estuary)
    
    write.csv(DO_year_estuary, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/', 
                                             basin,
                                             "/",
                                             'DO_Estuary_Yearround_IR_categories_', basin, '.csv'),
              na = "", row.names = FALSE)
    
    
  }
  
}
  
  
  
  # These are all in one files ----------------------------------------------
  
  
  
  # tox AL - copper ---------------------------------------------------------
  # all file
  
  
  if(exists('tox_al_copper')){
    rm(tox_al_copper)
  }
  print("Starting tox AL copper")
  tox_al_copper <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/TOX_AL_CU_BLM_IR_Categories_ALLDATA.csv", 
                            stringsAsFactors = FALSE)
  
  tox_al_copper <- Fix_AUs(tox_al_copper)
  
  write.csv(tox_al_copper, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/TOX_AL_CU_BLM_IR_Categories_ALLDATA.csv",
            na = "", row.names = FALSE)
  
  #biocriteria
  print("Starting biocriteria")
  
  
  
  
  # Biocriteria -------------------------------------------------------------
  
  
  if(exists('biocriteria')){
    rm(biocriteria)
  }
  biocriteria <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/MWCF_Proposed_withnotes_CSV.csv",
                          stringsAsFactors = FALSE)
  biocriteria <- Fix_AUs(biocriteria)
  
  write.csv(biocriteria,
            file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/MWCF_Proposed_withnotes_CSV.csv",
            na = "", row.names = FALSE)
  
  
  # narrative ---------------------------------------------------------------
  
  print("Starting narrative")
  if(exists('narrative')){
    rm(narrative)
  }
  narrative <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/narrative assessment put together.csv",
                        stringsAsFactors = FALSE) 
  
  narrative <- Fix_AUs(narrative)
  
  write.csv(narrative, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/narrative assessment put together.csv",
            na = "", row.names = FALSE)
  
  
  print("Starting shellfish")
  if(exists('shellfish_toxins')){
    rm(shellfish_toxins)
  }
  shellfish_toxins <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/shellfish_categories.xlsx") 
  
  
  shellfish_toxins <- Fix_AUs(shellfish_toxins)
  
  write.xlsx(shellfish_toxins, 
            file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/shellfish_categories.xlsx")
  
  
  
  # OA ----------------------------------------------------------------------
  ocean_listings <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/OA_categories.xlsx")
  
  ocean_listings <- Fix_AUs(ocean_listings)
  
  write.xlsx(ocean_listings, 
             file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/OA_categories.xlsx")
  
  # Ocean hypoxia -----------------------------------------------------------
  
  marine_oxygen <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/Ocean_DO_categories.xlsx") 
  
  
  marine_oxygen <- Fix_AUs(marine_oxygen)
  
  write.xlsx(marine_oxygen, 
             file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/01New/Ocean_DO_categories.xlsx")
  # put it all together -----------------------------------------------------
 

################################################################################
################################################################################  

  # Fix data files ----------------------------------------------------------

  
  
  #Read in AU adjustments
  AU_Fixes <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/AU_Fixes.xlsx") %>%
    select(OLD_AU_ID,NEW_AU_ID )
  
  
  #Replacement function
  Fix_AUs <- function(df){
    
    df <- df %>%
      left_join(AU_Fixes, by = c('AU_ID' = 'OLD_AU_ID')) %>%
      mutate(AU_ID = ifelse(!is.na(NEW_AU_ID), NEW_AU_ID, AU_ID)) %>%
      select(-NEW_AU_ID)
    
    return(df)
  }
  
  
  
data_files <-  list.files(path = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data", full.names = TRUE)
  
#Limit to csv files only
data_files <- data_files[grepl(".csv", data_files)]

for (i in 1:length(data_files)) {  

print(paste("Begin", str_split(data_files[[i]], pattern = 'Data')[[1]][2]))
  
 data <-  read.csv(data_files[[i]], stringsAsFactors = FALSE)
 
 data_fixed <- Fix_AUs(data)
 
 print("Write data file")
 
 write.csv(data_fixed, file = paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/New/", 
                                     str_split(data_files[[i]], pattern = '/')[[1]][9]),
           na = "", row.names = FALSE)

 
}
  

  