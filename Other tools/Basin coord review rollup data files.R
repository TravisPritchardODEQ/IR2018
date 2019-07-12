library(tidyverse)
library(openxlsx)

options(scipen = 999999999)

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

# for (i in 1:length(Basins)) {
#   
#   basin <- Basins[i]
# dir.create(paste0('//deqhq1/WQ-SHARE/2018 IR/Internal Review/Basin Summaries/', basin))
# }


bacteria_coast_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Coast_Contact_IR_Data_ALLDATA.csv", 
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

bacteria_fresh_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Fresh_Contact_IR_data_ALLDATA.csv",
                                       stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

bacteria_Shell_harvest <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Shell_harvest_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

chl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/chl_a/Data_Review/Chla_IR_data_ALLDATA.csv",
                stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

DO_cont_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Continuous_Spawn_IR_data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D)

DO_cont_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_YearRound_continuous_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

DO_instant_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D)

DO_inst_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_YearRound_instant_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

DO_estuary_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_estuary_instant_Spawn_IR_data_ALLDATA.csv",
                                     stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

DO_estuary_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Estuary_Yearround_IR_data_ALLDATA.csv",
                                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

pH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/pH/Data_Review/pH_IR_data_ALLDATA.csv",
               stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

temp <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Temperature/Data_Review/Temperature_IR_data_ALLDATA.csv",
                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  mutate(Spawn_criteria = ifelse(Spawn_type == "Spawn", 13, "" ) )

temp <- temp[,c(1:12, 50, 13:49)]

Tox_AL_Ammonia <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Ammonia_IR_Data_ALLDATA.csv",
                           stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_CU <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Cu_IR_Data_ALLDATA.csv",
                      stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Hardness_Metals <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Hardness_Metals_IR_Data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Others <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Others_IR_Data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Penta <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Pentachlorophenol_IR_data_ALLDATA.csv",
                         stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_HH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_HH/Data_Review/Tox_HH_IR_data_ALLDATA.csv",
                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_HH_Hg_tissue <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_HH/Data_Review/Tox_HH_hg_tissue_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)



# loop through each basin folder and get the assessed data. Write some preliminary tables

for (i in 1:length(Basins)) {
  
  rm(bacteria_coast_contact_basin,
     bacteria_fresh_contact_basin,
     bacteria_Shell_harvest_basin,
     chl_basin,
     DO_cont_spawn_basin,
     DO_cont_yearround_basin,
     DO_instant_spawn_basin,
     DO_inst_yearround_basin,
     DO_estuary_spawn_basin,
     DO_estuary_yearround_basin,
     pH_basin,
     temp_basin,
     Tox_AL_Ammonia_basin,
     Tox_AL_CU_basin,
     Tox_AL_Hardness_Metals_basin,
     Tox_AL_Others_basin,
     Tox_AL_Penta_basin,
     Tox_HH_basin,
     Tox_HH_Hg_tissue_basin)
  
  basin <- Basins[i]
  print(paste("Starting Basin-", basin))
  bacteria_coast_contact_basin <-  bacteria_coast_contact %>%
    filter(OWRD_Basin == basin)
  
  
  if(exists('bacteria_coast_contact_basin')){
    write.xlsx( bacteria_coast_contact_basin, 
                file = paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Bacteria_Coast_Contact.xlsx"), 
                overwrite = TRUE)
    
    }
  
  
  bacteria_fresh_contact_basin <-   bacteria_fresh_contact %>%
    filter(OWRD_Basin == basin)
  
  if(exists('bacteria_fresh_contact_basin')){
    write.xlsx( bacteria_fresh_contact_basin, 
                file = paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Bacteria_Fresh_Contact.xlsx"), 
                overwrite = TRUE)
    
  }
  
  
  bacteria_Shell_harvest_basin <-   bacteria_Shell_harvest %>%
    filter(OWRD_Basin == basin)
  
  
  if(exists('bacteria_Shell_harvest_basin')){
    write.xlsx( bacteria_Shell_harvest_basin, 
                file = paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Bacteria_Shellfish_Harvest.xlsx"), 
                overwrite = TRUE)
    
  }
  
  
  chl_basin <-   chl %>%
    filter(OWRD_Basin == basin)
  
  if(exists('chl_basin')){
    write.xlsx( chl_basin, 
                file = paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Chlorophyll.xlsx"), 
                overwrite = TRUE)
    
  }
  
  
  DO_cont_spawn_basin <-   DO_cont_spawn %>%
    filter(OWRD_Basin == basin)
  
  DO_cont_yearround_basin <-   DO_cont_yearround %>%
    filter(OWRD_Basin == basin)
  
  DO_instant_spawn_basin <-   DO_instant_spawn %>%
    filter(OWRD_Basin == basin)
  
  DO_inst_yearround_basin <-   DO_inst_yearround %>%
    filter(OWRD_Basin == basin)
  
  DO_estuary_spawn_basin <-   DO_estuary_spawn %>%
    filter(OWRD_Basin == basin)
  
  DO_estuary_yearround_basin <-   DO_estuary_yearround %>%
    filter(OWRD_Basin == basin)
  
  pH_basin <-   pH %>%
    filter(OWRD_Basin == basin)
  
  if(exists('pH_basin')){
    write.xlsx( pH_basin, 
                file = paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_pH.xlsx"), 
                overwrite = TRUE)
    
  }
  
  
  temp_basin <-   temp %>%
    filter(OWRD_Basin == basin)
  
  if(exists('temp_basin')){
    write.xlsx( temp_basin, 
                file = paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Temperature.xlsx"), 
                overwrite = TRUE)
    
  }
  
  Tox_AL_Ammonia_basin <-   Tox_AL_Ammonia %>%
    filter(OWRD_Basin == basin) 
  
  Tox_AL_CU_basin <-   Tox_AL_CU %>%
    filter(OWRD_Basin == basin)
  
  Tox_AL_Hardness_Metals_basin <-   Tox_AL_Hardness_Metals %>%
    filter(OWRD_Basin == basin)
  
  Tox_AL_Others_basin <-   Tox_AL_Others %>%
    filter(OWRD_Basin == basin)
  
  Tox_AL_Penta_basin <-   Tox_AL_Penta %>%
    filter(OWRD_Basin == basin)
  
  Tox_HH_basin <-   Tox_HH %>%
    filter(OWRD_Basin == basin)
  
  Tox_HH_Hg_tissue_basin <-   Tox_HH_Hg_tissue %>%
    filter(OWRD_Basin == basin)
  
  
  wb <- createWorkbook()
  addWorksheet(wb, "DO_spawn_continuous")
  addWorksheet(wb, "DO_spawn_instantaneous")
  
  writeData(wb,"DO_spawn_continuous",  DO_cont_spawn_basin, rowNames = FALSE)
  writeData(wb,"DO_spawn_instantaneous", DO_instant_spawn_basin, rowNames = FALSE)
  
  saveWorkbook(wb, paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_DO_Spawning.xlsx"), 
               overwrite = TRUE)
  
  wb <- createWorkbook()
  addWorksheet(wb, "DO_yearround_continuous")
  addWorksheet(wb, "DO_yearround_instantaneous")
  
  writeData(wb,"DO_yearround_continuous",  DO_cont_yearround_basin, rowNames = FALSE)
  writeData(wb,"DO_yearround_instantaneous", DO_inst_yearround_basin, rowNames = FALSE)
  
  saveWorkbook(wb, paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_DO_Yearround.xlsx"), 
               overwrite = TRUE)
  
  wb <- createWorkbook()
  addWorksheet(wb, 'Tox_AL_Others')
  addWorksheet(wb,'Tox_AL_Ammonia')
  addWorksheet(wb,'Tox_AL_CU')
  addWorksheet(wb, 'Tox_AL_Hardness_Metals')
  addWorksheet(wb, 'Tox_AL_Pentachlorophenol')
  
  
  if(exists('Tox_AL_Others_basin')){
    writeData(wb,  'Tox_AL_Others', Tox_AL_Others_basin, rowNames = FALSE)
  }
  if(exists('Tox_AL_Ammonia_basin')){
    writeData(wb, 'Tox_AL_Ammonia',  Tox_AL_Ammonia_basin, rowNames = FALSE)
  }
  if(exists('Tox_AL_CU_basin')){
    writeData(wb, 'Tox_AL_CU',  Tox_AL_CU_basin, rowNames = FALSE)
  }
  
  if(exists('Tox_AL_Hardness_Metals_basin')){
    writeData(wb, 'Tox_AL_Hardness_Metals',  Tox_AL_Hardness_Metals_basin, rowNames = FALSE)
  }
  
  if(exists('Tox_AL_Penta_basin')){
    writeData(wb, 'Tox_AL_Pentachlorophenol',  Tox_AL_Penta_basin, rowNames = FALSE)
  }
  
  
  saveWorkbook(wb, paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Aquatic_Life_Toxics.xlsx"), 
               overwrite = TRUE)
  
  
  
  
  wb <- createWorkbook()
  addWorksheet(wb, 'Tox_HH')
  addWorksheet(wb,'Tox_HH_Hg_Tissue')
  
  
  if(exists('Tox_HH_basin')){
    writeData(wb, 'Tox_HH',  Tox_HH_basin, rowNames = FALSE)
  }
  
  if(exists('Tox_HH_Hg_tissue_basin')){
    writeData(wb, 'Tox_HH_Hg_Tissue',  Tox_HH_Hg_tissue_basin, rowNames = FALSE)
  }
  
  
  saveWorkbook(wb, paste0("//deqhq1/WQ-Share/2018 IR/Internal Review/Basin Summaries/", basin,"/", basin, "_Human_Health_Toxics.xlsx"), 
               overwrite = TRUE)
  
  
}
    

