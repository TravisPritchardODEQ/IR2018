library(tidyverse)
library(openxlsx)

dictionary <- read.csv("ATTAINS/categories_dictionary.csv", stringsAsFactors = FALSE)

delistings_v4 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ALL BASINS_delistingsv8.csv",
                          stringsAsFactors = FALSE) %>%
  rename(Delist = Delisting.AGREE..) %>%
  select(-Temp...Seasonality) %>%
  select( AU_ID,
          Char_Name,
          Pollu_ID,
          WQstd_code,
          Period,
          Delist,
          Category_Final,
          Rationale) %>%
  mutate(Period = ifelse(Period == "", NA, Period ))

delistings_v4[] <- lapply(delistings_v4, as.character)

wqstrd_to_datafile <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/WQSTRD_to_datafile.csv",
                               stringsAsFactors = FALSE) %>%
  mutate(WQstd_code = as.character(WQstd_code))
# Setup -------------------------------------------------------------------

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



#save(Pollu_IDs, file = "ATTAINS/LU_Pollutant.Rdata")



# Table to assign Pollu_IDs to pollutants
load("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU_Pollutant.Rdata")
#save(Pollu_IDs, file ="ATTAINS/LU_Pollutant.Rdata" )


# This table connects the Pollu_IDs and WQstrd codes to beneficial uses.
# This is how we assign uses to assessments
BUs <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU Bus.csv",
                stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = as.character(WQstd_code))


# This table assigns Benuse codes to Assessment units.
# This is how we know what benefical uses are given for an assessment unit
AU_to_ben_use <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_names.csv",
                          stringsAsFactors = FALSE) %>%
  select(AU_ID, AU_UseCode) %>%
  mutate(AU_UseCode = as.character(AU_UseCode))

# Renames the columns to more common names
names(AU_to_ben_use) <- c("AU_ID", "ben_use_code")




# Before we brought pre 2018 assessments in, I needed to get basins to the biocriteria
# data. this is a bit obsolete now, but I didn't want ot rewrite it. 
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

LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')


names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

LU_benuses$ben_use_code <- as.character(LU_benuses$ben_use_code)

# This is a long form table of all the benefical uses that apply to a given AU
all_ben_uses <- AU_to_ben_use %>%
  left_join(LU_benuses) %>%
  filter(!is.na(ben_use),
         ben_use != "NULL")


DBI::dbDisconnect(con)

# This is the more current way to get what basin each AU is in
AU_ID_2_basin <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_2012_OWRD.csv",
                          stringsAsFactors = FALSE)



# Bring in actions for assigning category 4s
AU_Action <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_Action.csv", 
                      stringsAsFactors = FALSE) %>%
  mutate(Action_ID = as.character(Action_ID))

Action_Parameter <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/Action_Parameter.csv",
                             stringsAsFactors = FALSE) %>%
  mutate(ACTION_ID = as.character(ACTION_ID),
         Pollu_ID = as.character(Pollu_ID) )

DEQ_Actions <- AU_Action %>%
  left_join(Action_Parameter, by = c('Action_ID' = 'ACTION_ID'))

Action_names <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/actions.csv",
                         stringsAsFactors = FALSE) %>%
  mutate(ACTION_ID = as.character(ACTION_ID)) %>%
  select(ACTION_ID, ACTION_NAME) %>%
  rename(TMDL_Name = ACTION_NAME)

DEQ_Actions_names <-DEQ_Actions %>%
  left_join(Action_names, by = c('Action_ID' = 'ACTION_ID')) 



AU_names <-read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_names.csv",
                                     stringsAsFactors = FALSE) %>%
  select(AU_ID, AU_Name, AU_Description) 


# Create various lists used for combining data from each basin
put_together_list <- list()
delist_list <- list()
BU_rollup_list <- list()
BU_counts_list <- list()


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
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                       basin,
                       "/",
                       'Temperature_IR_categorization_',basin, '_with_validation_error_fix.csv'))) {
    
    temp <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
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
             Rationale
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
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Bacteria_Fresh_Contact_IR_Categories_',basin, '.csv'))) {
    
    fresh_contact <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
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
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  
  
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
             Data_Review_Comment,
             Rationale
  ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
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
             Data_Review_Comment,
             Rationale
             
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
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
             Rationale
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
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Ammonia_IR_Categories_',basin, '.csv'))) {
    
    
    
    #tox al - ammonia
    tox_al_ammonia <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
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
             Data_Review_Comment,
             Rationale
             
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
  tox_al_copper <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/TOX_AL_CU_BLM_IR_Categories_ALLDATA.csv", stringsAsFactors = FALSE) %>%
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
           Data_Review_Comment,
           Rationale) %>%
    mutate(Data_Review_Code = as.character(Data_Review_Code),
           Data_Review_Comment = as.character(Data_Review_Comment))
  
  
  
  #biocriteria
  print("Starting biocriteria")
  

  

# Biocriteria -------------------------------------------------------------

  
  if(exists('biocriteria')){
    rm(biocriteria)
  }
  biocriteria <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/MWCF_Proposed_withnotes_CSV.csv",
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
  narrative <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/narrative assessment put together.csv",
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
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Hardness_Metals_IR_Categories_',basin, '.csv'))) {
  
  #toxal - hardness
  
  tox_al_hardness <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
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
           Rationale
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
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'TOX_AL_Pentachlorophenol_IR_categories_',basin, '.csv'))) {
  
  toxal_penta <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
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
           Rationale
    ) %>%
    mutate(Data_Review_Code = as.character(Data_Review_Code),
           Data_Review_Comment = as.character(Data_Review_Comment))
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
             Rationale
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
  
  
  if (file.exists(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                         basin,
                         "/",
                         'Tox_HH_IR_Categories_',basin, '.csv'))) {
    
    
    
    
    tox_hh <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/', 
                              basin,
                              "/",
                              'Tox_HH_IR_Categories_',basin, '.csv'), 
                       stringsAsFactors = FALSE) %>%
      mutate(keep = ifelse(Pollu_ID == '50' & Char_Name != "p,p'-DDT", 0, 1 )) %>%
      filter(keep == 1) %>%
      select(-keep) %>%
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment)) %>%
      distinct()
    
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
  
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
             Data_Review_Comment,
            Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
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
             Data_Review_Comment,
             Rationale
      ) %>%
      mutate(Data_Review_Code = as.character(Data_Review_Code),
             Data_Review_Comment = as.character(Data_Review_Comment))
    
  }
  
  
  print("Starting shellfish")
  if(exists('shellfish_toxins')){
    rm(shellfish_toxins)
  }
  shellfish_toxins <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/shellfish_categories.xlsx") %>%
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
           Data_Review_Comment,
           Rationale
    ) 
  
  
  

# OA ----------------------------------------------------------------------
  ocean_listings <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/OA_categories.xlsx") %>%
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
           Data_Review_Comment,
           Rationale
    ) 
  

# Ocean hypoxia -----------------------------------------------------------

marine_oxygen <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Assessments/Ocean_DO_categories.xlsx") %>%
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
           Data_Review_Comment,
           Rationale
    ) 
  
# put it all together -----------------------------------------------------
print('Writing tables')
  
 # Get previous category 5 listings. 
   
  listings_2012 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Crosswalk/2012Crosswalk_Final.csv",
                            stringsAsFactors = FALSE) %>%
    select(ASSESSMENT_UNIT_ID, Pollu_ID,WQstrd_code, PARAM_ATTAINMENT_CODE, Time_Period) %>%
    rename(AU_ID = ASSESSMENT_UNIT_ID,
           WQstd_code = WQstrd_code,
           Period = Time_Period) %>%
    mutate(Pollu_ID = as.character(Pollu_ID),
           Period = case_when(Period == "Year_Round" ~ "Year Round",
                              Period == "Spawn" ~ "Spawning",
                              TRUE ~ Period)) %>%
    mutate(Period = ifelse(Period == "", NA, Period )) %>%
    distinct()%>%
    mutate(WQstd_code = as.character(WQstd_code)) %>%
    left_join(AU_ID_2_basin) %>%
    filter(OWRD_Basin == basin)
  
  
  previous_listings <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Crosswalk/2012Crosswalk_Final.csv",
                                stringsAsFactors = FALSE) %>%
    select(ASSESSMENT_UNIT_ID, Pollu_ID, PARAM_ATTAINMENT_CODE,WQstrd_code, PARAM_YEAR_LISTED, PARAM_NAME, Time_Period) %>%
    rename(Period = Time_Period) %>%
    distinct() %>%
    mutate(previous_IR_category = "Category 5",
           Pollu_ID = as.character(Pollu_ID),
           Period = case_when(Period == "Year_Round" ~ "Year Round",
                              Period == "Spawn" ~ "Spawning",
                              TRUE ~ Period)) %>% 
    mutate(Period = ifelse(Period == "", NA, Period )) %>%
    left_join(select(Pollutants, Pollu_ID, Pollutant_DEQ.WQS), by = "Pollu_ID") %>%
    mutate(PARAM_NAME = Pollutant_DEQ.WQS) %>%
    select(-Pollutant_DEQ.WQS) %>%
    rename(AU_ID = ASSESSMENT_UNIT_ID,
           WQstd_code = WQstrd_code) %>%
    mutate(WQstd_code = as.character(WQstd_code)) %>%
    left_join(AU_ID_2_basin, by = "AU_ID") %>%
    filter(OWRD_Basin == basin) %>%
    group_by(AU_ID, Pollu_ID, WQstd_code) %>%
    filter(PARAM_YEAR_LISTED == min(PARAM_YEAR_LISTED, na.rm = TRUE))
    
      
  
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
                                    get0('ocean_listings'),
                                    get0('marine_oxygen')) %>%
    filter(Char_Name != "Endrin + cis-Nonachlor") %>%
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
                              Period == "Spawn" ~ "Spawning",
                              TRUE ~ Period)) %>%
    rename(analysis_comment_2018 = analysis_comment) %>%
    filter(AU_ID != "")
  
 
  
  #read in manually reviewed delistings
  
  delist_reviewed <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ALL BASINS_delistingsv8.csv",
                              stringsAsFactors = FALSE) %>%
    mutate(Pollu_ID = as.character(Pollu_ID),
           WQstd_code = as.character(WQstd_code)) %>%
    filter(OWRD_Basin == basin) %>%
    select(AU_ID, Pollu_ID, WQstd_code, Period, Delisting.AGREE.., Category_Final,Rationale ) %>%
    mutate(Period = ifelse(Period == "", NA, Period )) %>%
    mutate(Period = as.character(Period),
           Delisting.AGREE.. = trimws(Delisting.AGREE..),
           Period = case_when(Period %in% c("Year_Round", "Year round") ~ "Year Round",
                              Period == "Spawn" ~ "Spawning",
                              TRUE ~ Period)) %>%
    rename(Delist = Delisting.AGREE..,
           Delist_rationale = Rationale)
  
  # There is some wonly join here iwth AU name - fix this. 
  ####################
  ###################
  ##################
  all_assessments <- put_together_initial %>%
    mutate(year_assessed = '2018') %>%
    full_join(previous_listings, by = c('Pollu_ID', 'AU_ID', 'WQstd_code', 'OWRD_Basin', 'Period')) %>%
    mutate(Assessed_in_2018 = ifelse(is.na(year_assessed), "NO", 
                                     ifelse(year_assessed == '2018', "YES", NA )),
           assessment_result_2018 = ifelse(year_assessed == '2018', IR_category, NA),
           PARAM_YEAR_LISTED = as.character(PARAM_YEAR_LISTED)) %>%
    mutate(Char_Name = ifelse(is.na(Char_Name), PARAM_NAME, Char_Name ),
           PARAM_YEAR_LISTED = case_when(is.na(PARAM_YEAR_LISTED) & IR_category == "Category 5" ~ '2018',
                                         !is.na(PARAM_YEAR_LISTED) ~ PARAM_YEAR_LISTED )) %>%
    select(-PARAM_ATTAINMENT_CODE, -PARAM_NAME) %>%
    left_join(delist_reviewed, by = c("AU_ID", "Pollu_ID", "WQstd_code", "Period")) %>%
    # mutate(IR_category = case_when(previous_IR_category == 'Category 5' & assessment_result_2018 == "Category 2"  ~ "Category 2",
    #                                 previous_IR_category == 'Category 5' ~'Category 5',
    #                                 TRUE ~ assessment_result_2018)) %>%
    mutate(IR_category = ifelse(is.na(IR_category) & 
                                  Assessed_in_2018 == 'NO', 'Category 5', IR_category )) %>%
    mutate(IR_category = ifelse(!is.na(previous_IR_category), previous_IR_category, IR_category )) %>%
    mutate(IR_category = case_when(!is.na(Category_Final) ~ Category_Final,
                                   TRUE ~ IR_category)) %>%
    mutate(Rationale = ifelse(grepl("no", Delist, ignore.case = TRUE), Delist_rationale, Rationale )) %>%
    # mutate(IR_category = case_when(previous_IR_category == 'Category 5' & (Delist != 'YES' | is.na(Delist)) ~ "Category 5",
    #                                TRUE ~ IR_category
    #                                )) %>%
    #rename(Year_listed = PARAM_YEAR_LISTED) %>%
    mutate(Year_listed = ifelse(IR_category != "Category 5", NA, PARAM_YEAR_LISTED )) %>%
    #left_join(Pollu_IDs, by = c('Pollu_ID' = 'LU_Pollu_ID')) %>%
    #mutate(Char_Name = ifelse(is.na(assessment_result_2018) & !is.na(LU_Pollutant), LU_Pollutant, Char_Name )) %>%
    #select(-LU_Pollutant) %>%
    left_join(AU_names, by = "AU_ID") 
  
  

  all_assessments <-  all_assessments[,c(1,22,23,2,3,4,5,6,7,8,9,10,11,12,13,21,19,15,16,17,18)]
    
  
  IR_category_factor <- factor(all_assessments$IR_category, levels = c('Unassigned',
                                                                            "-",
                                                                            "Category 3C",
                                                                            "Category 3D",
                                                                            "Category 3",
                                                                            "Category 3B",
                                                                            "Category 2",
                                                                            "Category 4A",
                                                                            "Category 5"),
                               ordered = TRUE)
  
  all_assessments$IR_category <- IR_category_factor
  
  
  put_together <- all_assessments %>%
    # mutate(Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
    #                              Char_Name == "Alkalinity, bicarbonate" ~ 'Alkalinity',
    #                              Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
    #                              Char_Name == 'DDT' ~ "DDT 4,4'",
    #                              Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
    #                              TRUE ~ Char_Name)) %>%
    # left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
    # mutate(Pollu_ID = ifelse(is.na(Pollu_ID) | Pollu_ID == "", LU_Pollu_ID, Pollu_ID )) %>%
    # select(-LU_Pollu_ID) %>%
    arrange(AU_ID) %>%
    left_join(select(Pollutants, Pollu_ID, Pollutant_DEQ.WQS),by = "Pollu_ID") %>%
    mutate(Char_Name = ifelse(!is.na(assess_type), paste(Pollutant_DEQ.WQS, "-",assess_type), Pollutant_DEQ.WQS )) %>%
    select(-Pollutant_DEQ.WQS, - assess_type)
  
  
  cat4_categories_other_than_B <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Crosswalk/2012Crosswalk_Final.csv",
                                           stringsAsFactors = FALSE) %>%
    select(ASSESSMENT_UNIT_ID, Pollu_ID, WQstrd_code, DEQ_Cat) %>%
    filter(DEQ_Cat != "") %>%
    rename(WQstd_code = WQstrd_code,
           AU_ID = ASSESSMENT_UNIT_ID) %>%
    mutate(WQstd_code = as.character(WQstd_code),
           Pollu_ID = as.character(Pollu_ID))

  # x_walk_impaired <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Crosswalk/x_walk_impaired.csv",
  #                             stringsAsFactors = FALSE) %>%
  #   mutate(Period = case_when(Period == 'YearRound' ~ "Year Round",
  #                             Period == 'Spawning' ~ "Spawning",
  #                             TRUE ~ Period))
    
  
  
  cat4_assignments <- put_together %>%
    left_join(DEQ_Actions_names, by = c("AU_ID", "Pollu_ID")) %>%
    mutate(IR_category = ifelse(IR_category == "Category 5" & !is.na(Action_ID), "Category 4A", as.character(IR_category) )) %>%

    mutate(Review_Comment = "",
           Revised_Category = "") %>%
    left_join(cat4_categories_other_than_B) %>%
    mutate(IR_category = ifelse(!is.na(DEQ_Cat), DEQ_Cat, IR_category )) %>%
    select(-DEQ_Cat)
  
  basin_categories <- cat4_assignments %>%
    mutate(year_assessed = ifelse(is.na(year_assessed), Year_listed, year_assessed )) %>%
    distinct() %>%
    group_by(AU_ID, AU_Name, AU_Description, Char_Name, Pollu_ID, WQstd_code,
             Period, OWRD_Basin, IR_category, analysis_comment_2018, 
             Data_Review_Code,Data_Review_Comment, Rationale, year_assessed, Year_listed,
             previous_IR_category,Assessed_in_2018, assessment_result_2018 ) %>%
    summarise(Action_ID = ifelse(length(str_c(Action_ID, collapse  = "; ")) > 0, str_c(Action_ID, collapse  = "; "), ""),
              TMDL_Name = ifelse(length(str_c(TMDL_Name, collapse  = "; ")) > 0, str_c(TMDL_Name, collapse  = "; "), "")) %>%
    mutate(Review_Comment = "",
           Revised_Category = "") %>%
    left_join(wqstrd_to_datafile, by = "WQstd_code")
  
  basin_categories <-  basin_categories[,c(1,2,3,4,5,6,23, 7:22)]
    
  
  
  
  #set up excel sheets
  
  wb <- createWorkbook()
  addWorksheet(wb, "IR Categories")
  addWorksheet(wb, "Dictionary")
  
  writeData(wb,"IR Categories",  basin_categories , rowNames = FALSE)
  writeData(wb,"Dictionary", dictionary, rowNames = FALSE)
  
  saveWorkbook(wb, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_categories.xlsx"), 
               overwrite = TRUE)
  
  
  # write.csv(basin_categories, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_categories.csv"),
  #           row.names = FALSE,
  #           na = "")
  # 
 
  
  
  
  crosswalked <- cat4_assignments %>%
    ungroup() %>%
    # mutate(Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
    #                              Char_Name == "Alkalinity, bicarbonate" ~ 'Alkalinity',
    #                              Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
    #                              Char_Name == 'DDT' ~ "DDT 4,4'",
    #                              Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
    #                              TRUE ~ Char_Name)) %>%
    #filter(IR_category != "Category 5" & (previous_IR_category == "Category 5" | previous_IR_category == "Category 4A")) %>%
    
    left_join(select(delistings_v4, -Char_Name), by = c("AU_ID", "Pollu_ID", "WQstd_code", "Period", "Category_Final", "Delist")) %>%
    filter(IR_category == "Category 2") 
    
  #   
  # 
  # crosswalked <- put_together_initial %>%
  #   mutate(Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
  #                                Char_Name == "Alkalinity, bicarbonate" ~ 'Alkalinity',
  #                                Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
  #                                Char_Name == 'DDT' ~ "DDT 4,4'",
  #                                Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
  #                                TRUE ~ Char_Name)) %>%
  #   left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
  #   mutate(Pollu_ID = ifelse(is.na(Pollu_ID) | Pollu_ID == "", LU_Pollu_ID, Pollu_ID )) %>%
  #   select(-LU_Pollu_ID) %>%
  #   arrange(AU_ID) %>%
  #   left_join(listings_2012) %>%
  #   filter(grepl('2', IR_category),
  #          !is.na(PARAM_ATTAINMENT_CODE)) %>%
  #   rename(Previous_PARAM_ATTAINMENT_CODE = PARAM_ATTAINMENT_CODE)
  
  
  write.csv(crosswalked, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_delistings.csv"),
                                 row.names = FALSE,
                                 na = "")
  
  
  
  BU_s <- cat4_assignments %>%
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
  
  
  put_together_list[[i]] <- basin_categories
  delist_list[[i]] <- crosswalked
  BU_rollup_list[[i]] <- BU_s
  BU_counts_list[[i]] <- BU_counts
# End ---------------------------------------------------------------------

  
  }
    
  
    
 all_categories <-bind_rows(put_together_list)   
 all_delist <- bind_rows(delist_list)  
 all_BU_rollup <- bind_rows(BU_rollup_list) 
 all_BU_counts <- bind_rows(BU_counts_list)
 
 
 all_categories <- all_categories %>%
   ungroup() %>%
   mutate(Rationale = ifelse(is.na(Rationale), '', Rationale )) %>%
   mutate(Rationale = ifelse(Assessed_in_2018 == 'NO', "Carried forward from previous listing", Rationale )) %>%
   mutate(Rationale = ifelse(Rationale == '', "Carried forward from previous listing", Rationale ))
 
 AU_to_OWRD <- all_categories %>%
   ungroup() %>%
   select(AU_ID, OWRD_Basin) %>%
   distinct()
 
 save(AU_to_OWRD, file =  'E:/Documents/IR_Display/data/AU_to_OWRD.Rdata')
    

 OWRD_basins <- all_categories %>%
   distinct(AU_ID, OWRD_Basin)
 
cat_factor <- factor(all_BU_rollup$IR_category, levels = c("Use not assessed",
                                                                                      "Category 3C",
                                                                                      "Category 3D",
                                                                                      "Category 3",
                                                                                      "Category 3B",
                                                                                      "Category 2",
                                                                                      "Category 4B",
                                                                                      "Category 4C",
                                                                                      "Category 4",
                                                                                      "Category 4A",
                                                                                      "Category 5"),
                           ordered = TRUE)
 
 
 
 
all_BU_rollup$IR_category <- cat_factor       
 
 
 BU_rollup <- all_BU_rollup %>%
   group_by(AU_ID, ben_use) %>%
   summarise(Category = max(IR_category)) %>%
   right_join(filter(all_ben_uses, AU_ID %in% all_categories$AU_ID)) %>%
   mutate(Category = as.character(Category),
          Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
   select(-ben_use_id) %>%
   #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
   spread(ben_use, Category, fill = "-") #%>%
   # select(-`Commercial Navigation and Transportation`,
   #        -`Hydro Power`, -`Industrial Water Supply`, -`Irrigation`,
   #        -`Livestock Watering`, -`Wildlife and Hunting`)
 
 
 BU_Summary <- all_BU_rollup %>%
   mutate(Year_listed = ifelse(is.na(Year_listed), year_assessed, Year_listed ),
          Parameter = ifelse(is.na(Period), Char_Name, paste0(Char_Name, "- ",Period ) )) %>%
   group_by(AU_ID, ben_use) %>%
   right_join(filter(all_ben_uses, AU_ID %in% all_categories$AU_ID)) %>%
   summarise(Assessed_condition = case_when(max(IR_category, na.rm = TRUE) == "Category 5" ~ "Not supported",
                                            max(IR_category, na.rm = TRUE) == "Category 4A" ~ "Not supported. TMDL in place",
                                            max(IR_category, na.rm = TRUE) == "Category 4" ~ " Data indicate that at least one designated use is not supported, but a TMDL is not needed to address the pollutant",
                                            max(IR_category, na.rm = TRUE) == "Category 4B" ~ "Pollution control requirements other than TMDLs are expected to address pollutant of concern and will result in attainment of water quality standards",
                                            max(IR_category, na.rm = TRUE) == "Category 4C" ~ "Impairment is caused by pollution, not a pollutant",
                                            max(IR_category, na.rm = TRUE) == "Category 2" ~ "Standards met for all assessed parameters",
                                            max(IR_category, na.rm = TRUE) == "Category 3B" ~ "Insufficient data to determine use support, but some data indicate non-attainment of a criterion",
                                            max(IR_category, na.rm = TRUE) == "Category 3" ~ "Insufficient data to determine use support",
                                            max(IR_category, na.rm = TRUE) == "Category 3D" ~ "Insufficient data to determine use support because numeric criteria are less than quantification limits",
                                            max(IR_category, na.rm = TRUE) == "Category 3C" ~ "Insufficient data to determine use support, but data indicated marginal bilogical condition",
                                            TRUE ~ "Use not assessed"),
             Impairment_cause = ifelse(Assessed_condition == "Not supported" |
                                         Assessed_condition == "Not supported. TMDL in place",str_c(unique(Parameter[IR_category ==  "Category 5" | IR_category ==  "Category 4A"]), collapse  = "; "), "" ),
             Parameters_assessed = ifelse(Assessed_condition != "Use not assessed", str_c(Parameter, collapse  = "; "), ""),
             year_listed = ifelse(Assessed_condition == "Not supported" |
                                    Assessed_condition == "Not supported. TMDL in place", min(Year_listed), '' )
   )
 
 # data display tables -----------------------------------------------------
 
 
 Impaired_1orMoreUses_prelim <-  all_BU_rollup %>%
   mutate(Name = "",
          Description = "",
          AU_Size = "") %>%
   mutate(Pollu_ID = ifelse(Pollu_ID == "160", "104", Pollu_ID )) %>%
   left_join(Pollutants,by = "Pollu_ID") %>%
   mutate(Year_listed = ifelse(is.na(Year_listed) | Year_listed == "", year_assessed, Year_listed ),
          Parameter = ifelse(is.na(Period), Char_Name, paste0(Char_Name, "- ",Period )),
          Parameter = ifelse(!is.na(Char_Name) & WQstd_code == "15",  paste0(Char_Name, "- ","Aquatic Life"), Parameter),
          Parameter = ifelse(!is.na(Char_Name) &  WQstd_code == "16",  paste0(Char_Name, "- ","Human Health"), Parameter),
          year_assessed = ifelse(is.na(year_assessed), Year_listed, year_assessed )) %>%
   right_join(filter(all_ben_uses, AU_ID %in% all_categories$AU_ID)) %>%
   group_by(AU_ID, ben_use) %>%
   mutate(Assessed_condition = case_when(max(IR_category, na.rm = TRUE) == "Category 5" ~ "Category 5",
                                         max(IR_category, na.rm = TRUE) == "Category 4" ~ "Category 4",
                                         max(IR_category, na.rm = TRUE) == "Category 4B" ~ "Category 4B",
                                         max(IR_category, na.rm = TRUE) == "Category 4C" ~ "Category 4C",
                                         max(IR_category, na.rm = TRUE) == "Category 4A" ~ "Category 4A",
                                max(IR_category, na.rm = TRUE) == "Category 2" ~ "Category 2",
                                max(IR_category, na.rm = TRUE) == "Category 3B" ~ "Category 3B",
                                max(IR_category, na.rm = TRUE) == "Category 3" ~ "Category 3",
                                max(IR_category, na.rm = TRUE) == "Category 3D" ~ "Category 3D",
                                max(IR_category, na.rm = TRUE) == "Category 3C" ~ "Category 3C",
                                TRUE ~ "Use not assessed"),
             Impairment_cause = ifelse(Assessed_condition == "Category 5" | Assessed_condition =="Category 4A", str_c(unique(Parameter[IR_category ==  "Category 5" | IR_category ==  "Category 4A"]), collapse  = "; "), NA ),
             Year_listed = min(Year_listed, na.rm = TRUE),
          parameters_assessed = str_c(unique(Pollutant_DEQ.WQS), collapse = "; "),
          parameter_group_assessed = str_c(unique(Attains_Group), collapse = "; ")) %>%
   ungroup() 
 
 
   
Impaired_factor <- factor(Impaired_1orMoreUses_prelim$Assessed_condition, levels = c("Use not assessed",
                                                                                     "Category 3C",
                                                                                     "Category 3D",
                                                                                     "Category 3",
                                                                                     "Category 3B",
                                                                                     "Category 2",
                                                                                     "Category 4B",
                                                                                     "Category 4C",
                                                                                     "Category 4",
                                                                                     "Category 4A",
                                                                                     "Category 5"),
                              ordered = TRUE)



 
Impaired_1orMoreUses_prelim$Assessed_condition <- Impaired_factor            
               
Impaired_1orMoreUses <- Impaired_1orMoreUses_prelim %>%
  mutate(condition = ifelse(grepl("3", Assessed_condition), 'Category 3', as.character(Assessed_condition))) %>%
  group_by(AU_ID) %>%
  summarise(Impaired_Uses = ifelse(length(str_c(unique(ben_use[condition ==  "Category 5" | 
                                                                 condition == "Category 4A" | 
                                                                 condition == "Category 4"  | 
                                                                 condition == "Category 4B" |  
                                                                 condition == "Category 4C" ]), collapse = "; ")) > 0,  
                                   str_c(unique(ben_use[condition ==  "Category 5"| condition ==  "Category 5" | 
                                                          condition == "Category 4A" | 
                                                          condition == "Category 4"  | 
                                                          condition == "Category 4B" |  
                                                          condition == "Category 4C"]), collapse = "; "), 
                                   '-' ),
            Impairment_cause = ifelse(length(str_c(unique(Parameter[IR_category ==  "Category 5" |  IR_category == "Category 4A" | 
                                                                      IR_category == "Category 4"  | 
                                                                      IR_category == "Category 4b" |  
                                                                      IR_category == "Category 4C"])[!is.na(unique(Parameter[IR_category ==  "Category 5" | IR_category == "Category 4A" | 
                                                                                                                             IR_category == "Category 4"  | 
                                                                                                                             IR_category == "Category 4C" |  
                                                                                                                             IR_category == "Category 4C"]))], 
                                                   collapse  = "; ")) > 0, 
                                      str_c(unique(Parameter[IR_category ==  "Category 5" | 
                                                               IR_category == "Category 4A" | 
                                                               IR_category == "Category 4"  | 
                                                               IR_category == "Category 4B" |  
                                                               IR_category == "Category 4C"])[!is.na(unique(Parameter[IR_category ==  "Category 5" | IR_category == "Category 4A" | 
                                                                                                                        IR_category == "Category 4"  | 
                                                                                                                        IR_category == "Category 4B" |  
                                                                                                                        IR_category == "Category 4C"]))],
                                            collapse  = "; "), 
                                      "."),
            year_listed = min(Year_listed, na.rm = TRUE),
            year_last_assessed = max(year_assessed, na.rm = TRUE),
            attaining_uses = ifelse(length(str_c(unique(ben_use[condition ==  "Category 2"]), collapse = "; ")) > 0, 
                                    str_c(unique(ben_use[condition ==  "Category 2"]), collapse = "; "), "-" ),
            insufficient_data_uses = ifelse(length(str_c(unique(ben_use[condition ==  "Category 3"]), collapse = "; ")) > 0, 
                                            str_c(unique(ben_use[condition ==  "Category 3"]), collapse = "; "), "-" ),
            unassessed_uses = str_c(unique(ben_use[condition ==  "Use not assessed"]), collapse = "; "),
            Parameters_assessed = str_c(unique(Pollutant_DEQ.WQS[!is.na(Pollutant_DEQ.WQS)]), collapse = "; "),
            parameter_group_assessed = str_c(unique(Attains_Group[!is.na(Attains_Group)]), collapse = "; "),
            TMDLs = ifelse(length(str_c(unique(TMDL_Name[!is.na(TMDL_Name)]), collapse = "; ")) > 0, 
                           str_c(unique(TMDL_Name[!is.na(TMDL_Name)]), collapse = "; "), "" ) 
            
  ) 

# parameter group ---------------------------------------------------------

long_BUs <- BUs %>%
  group_by(WQstd_code, Pollu_ID) %>%
  mutate(affected_uses = str_c(unique(ben_use), collapse = "; "))

Parameter <- all_categories %>%
  ungroup() %>%
  mutate(Pollu_ID = ifelse(Pollu_ID == "160", "104", Pollu_ID )) %>%
  left_join(Pollutants,by = "Pollu_ID") %>%
  mutate(year_assessed = ifelse(is.na(year_assessed), Year_listed, year_assessed )) %>%
  left_join(distinct(select(long_BUs, Pollu_ID, WQstd_code, affected_uses)),by = c("Pollu_ID", "WQstd_code")) %>%
  select(AU_ID, Char_Name,
         Pollu_ID, WQstd_code, Period,
         OWRD_Basin, IR_category,
         analysis_comment_2018, Data_Review_Comment,
         Rationale, year_assessed, Year_listed,
         previous_IR_category, Assessed_in_2018, 
         assessment_result_2018, Attains_Group, affected_uses)


# Count_impaired_pollutants -----------------------------------------------
Count_impaired_pollutants <- all_categories %>%
  group_by(AU_ID) %>%
  summarise(Count_impaired_pollutants = sum(IR_category == "Category 5" | IR_category == "Category 4A"),
            assessed_parameters = n_distinct(Char_Name)) %>%
  filter(AU_ID != "")


 all_delist <- all_delist %>%
   distinct()
    
 
 
 
 
 wb <- createWorkbook()
 addWorksheet(wb, "IR Categories")
 addWorksheet(wb, "Dictionary")
 
 writeData(wb,"IR Categories",  all_categories , rowNames = FALSE)
 writeData(wb,"Dictionary", dictionary, rowNames = FALSE)
 
 saveWorkbook(wb, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_categories.xlsx"), 
              overwrite = TRUE)
 
 # write.csv(all_categories, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_categories.csv"),
 #           row.names = FALSE,
 #           na = "")
 
 write.csv(Impaired_1orMoreUses, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_Impaired_1orMoreUses.csv"),
           row.names = FALSE,
           na = "")
 
 write.xlsx(Impaired_1orMoreUses, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_Impaired_1orMoreUses.xlsx"))

 write.csv(all_delist, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_delistings.csv"),
           row.names = FALSE,
           na = "")
 
 
 
 write.csv(BU_Summary, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_BU_summary.csv"),
           row.names = FALSE,
           na = "")
 
 write.csv(BU_rollup, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_BU_rollup.csv"),
           row.names = FALSE,
           na = "")

 write.csv(all_BU_counts, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_BU_counts.csv"),
           row.names = FALSE,
           na = "")
 

# write_display_tables ----------------------------------------------------


 write.csv(Parameter, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_Parameters.csv"),
           row.names = FALSE,
           na = "")
 write.csv(Count_impaired_pollutants, paste0("ATTAINS/Rollup/Basin_categories/", "ALL BASINS","_Count_impaired_pollutants.csv"),
           row.names = FALSE,
           na = "")
 
 

# Delist rollup -----------------------------------------------------------

delist_rollup <- all_delist %>%
   mutate(Char_Name = case_when(WQstd_code == '15' ~ paste(Char_Name, ("(AL)")), 
                                WQstd_code == '16' ~ paste(Char_Name, ("(HH)")),
                                TRUE ~Char_Name )) %>%
   mutate( Parameter = ifelse(is.na(Period), Char_Name, paste0(Char_Name, "- ",Period ) )) %>%
   group_by(AU_ID, AU_Name) %>%
   summarise(Delistings = str_c(unique(Parameter), collapse = "; "))
 
 
 write.csv(delist_rollup, file = "ATTAINS/Rollup/Basin_categories/ALL BASINS_delistings_AU_Rollup.csv")
 
 
 
 delistings <- delistings_v4 %>%
   filter(Category_Final == 'Category 2')
 
 write.xlsx(delistings, "ATTAINS/Rollup/Basin_categories/Delistings.xlsx")

# # Basin delisting files ---------------------------------------------------
# 
# delistings <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_delistings.csv", stringsAsFactors = FALSE)  
#  
#  
#  for (i in 1:length(Basins)) {
#    
#    
#    basin <- Basins[i]
#   
#    basin_delistings <- delistings %>%
#      filter(OWRD_Basin == basin,
#             Delist == "YES") %>%
#      mutate(WQstd_code = as.character(WQstd_code)) %>%
#      left_join(wqstrd_to_datafile, by = "WQstd_code")
#    
#    basin_delistings <- basin_delistings[,c(1,2,3,4,5,24, 6:23)]
#    
#    write.xlsx(basin_delistings, paste0("ATTAINS/Rollup/Basin_categories/", basin,"_delistings.xlsx"),
#              row.names = FALSE,
#              na = "")
#     
#    
#  }
 
 