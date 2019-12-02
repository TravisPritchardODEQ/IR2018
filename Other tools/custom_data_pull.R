library(tidyverse)
library(openxlsx)

options(scipen = 999999999)


name <- "Daniel_Turner_Data_Request"

AUs <- c('OR_SR_1708000309_04_100662',
         'OR_LK_1707010511_88_100135',
         'OR_LK_1707010102_88_100150',
         'OR_LK_1707010106_88_100133',
         'OR_LK_1707010106_88_100146',
         'OR_LK_1707010109_88_100144',
         'OR_LK_1707010114_88_100140',
         'OR_LK_1707010114_88_100141',
         'OR_LK_1707010114_88_100142',
         'OR_LK_1707010504_88_100137',
         'OR_LK_1707010504_88_100138',
         'OR_LK_1707010504_88_100139',
         'OR_LK_1707010511_88_100135',
         'OR_LK_1707010511_88_100136',
         'OR_LK_1707010512_88_100134',
         'OR_LK_1708000605_04_100320',
         'OR_LK_1708000605_04_100323',
         'OR_SR_1708000309_04_100662',
         'OR_LK_1707010504_88_100137',
         'OR_LK_1707010504_88_100138',
         'OR_SR_1708000309_04_100662',
         'OR_LK_1708000605_04_107234',
         'OR_SR_1708000309_04_100662',
         'OR_LK_1708000605_04_100320',
         'OR_LK_1708000605_04_100323',
         'OR_SR_1708000309_04_100663',
         'OR_SR_1708000309_04_100665',
         'OR_SR_1708000309_04_100666',
         'OR_SR_1708000309_04_100667',
         'OR_SR_1708000309_04_100668',
         'OR_SR_1708000309_04_100675',
         'OR_SR_1708000302_88_100669',
         'OR_LK_1708000605_04_107234',
         'OR_SR_1708000108_88_100673',
         'OR_SR_1708000309_04_100662',
         'OR_SR_1708000309_04_100664',
         'OR_LK_1708000605_04_100320',
         'OR_LK_1708000605_04_100323',
         'OR_SR_1708000309_04_100663',
         'OR_SR_1708000309_04_100665',
         'OR_SR_1708000309_04_100666',
         'OR_SR_1708000309_04_100667',
         'OR_SR_1708000309_04_100668',
         'OR_SR_1708000309_04_100675',
         'OR_SR_1708000108_88_100671',
         'OR_SR_1708000108_88_100672',
         'OR_SR_1708000108_88_100674',
         'OR_SR_1708000302_88_100669',
         'OR_LK_1708000605_04_100323',
         'OR_LK_1708000605_04_107234',
         'OR_SR_1708000309_04_100662',
         'OR_LK_1707010114_88_100131',
         'OR_LK_1707010504_88_100139',
         'OR_LK_1707010512_88_100134',
         'OR_LK_1707010114_88_100140',
         'OR_LK_1707010114_88_100141',
         'OR_LK_1707010114_88_100142',
         'OR_SR_1708000108_88_100674',
         'OR_LK_1707010106_88_100132',
         'OR_LK_1707010109_88_100144',
         'OR_LK_1707010109_88_100145',
         'OR_LK_1707010114_88_100143',
         'OR_LK_1707010106_88_100133',
         'OR_LK_1707010106_88_100146',
         'OR_LK_1707010114_88_100131',
         'OR_LK_1707010504_88_100137',
         'OR_LK_1707010504_88_100138',
         'OR_LK_1707010511_88_100135',
         'OR_LK_1707010511_88_100136',
         'OR_LK_1708000605_04_107234',
         'OR_SR_1708000108_88_100673',
         'OR_SR_1708000309_04_100662',
         'OR_SR_1708000309_04_100664',
         'OR_LK_1707010504_88_100139',
         'OR_LK_1707010114_88_100140',
         'OR_LK_1707010114_88_100141',
         'OR_LK_1707010114_88_100142',
         'OR_LK_1708000605_04_100323',
         'OR_SR_1708000309_04_100663',
         'OR_SR_1708000309_04_100665',
         'OR_SR_1708000309_04_100666',
         'OR_SR_1708000309_04_100667',
         'OR_SR_1708000309_04_100668',
         'OR_SR_1708000309_04_100675',
         'OR_SR_1708000108_88_100671',
         'OR_SR_1708000108_88_100672',
         'OR_SR_1708000108_88_100674',
         'OR_LK_1707010106_88_100132',
         'OR_LK_1707010109_88_100144',
         'OR_LK_1707010109_88_100145',
         'OR_LK_1707010114_88_100143',
         'OR_SR_1708000302_88_100669',
         'OR_SR_1708000108_88_100673',
         'OR_SR_1708000302_88_100670',
         'OR_SR_1708000108_88_100671',
         'OR_SR_1708000108_88_100672',
         'OR_SR_1708000108_88_100674',
         'OR_LK_1707010102_88_100147',
         'OR_LK_1707010106_88_100133',
         'OR_LK_1707010106_88_100146',
         'OR_LK_1707010114_88_100131',
         'OR_LK_1707010504_88_100138',
         'OR_LK_1708000605_04_107234',
         'OR_SR_1708000302_88_100670',
         'OR_LK_1707010504_88_100139',
         'OR_LK_1707010114_88_100140',
         'OR_LK_1707010114_88_100141',
         'OR_LK_1707010114_88_100142',
         'OR_LK_1707010102_88_100148',
         'OR_LK_1707010102_88_100149',
         'OR_LK_1707010102_88_100150',
         'OR_LK_1707010109_88_100144',
         'OR_LK_1707010109_88_100145',
         'OR_LK_1707010114_88_100143',
         'OR_SR_1708000302_88_100669',
         'OR_LK_1707010106_88_100133',
         'OR_LK_1707010106_88_100146',
         'OR_LK_1707010114_88_100131',
         'OR_LK_1707010504_88_100138',
         'OR_LK_1707010511_88_100135',
         'OR_LK_1707010511_88_100136',
         'OR_LK_1708000605_04_107234',
         'OR_LK_1707010504_88_100139',
         'OR_LK_1707010512_88_100134',
         'OR_LK_1707010114_88_100141',
         'OR_LK_1707010114_88_100142',
         'OR_SR_1708000309_04_100675',
         'OR_LK_1707010102_88_100150',
         'OR_LK_1707010109_88_100144',
         'OR_LK_1707010109_88_100145',
         'OR_LK_1707010114_88_100143'
)

bacteria_coast_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Coast_Contact_IR_Data_ALLDATA.csv", 
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

bacteria_fresh_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Fresh_Contact_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

bacteria_Shell_harvest <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Bacteria/Data Review/Bacteria_Shell_harvest_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

chl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/chl_a/Data_Review/Chla_IR_data_ALLDATA.csv",
                stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

DO_cont_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Continuous_Spawn_IR_data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D) %>%
  filter(AU_ID %in% AUs)

DO_cont_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_YearRound_continuous_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

DO_instant_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D) %>%
  filter(AU_ID %in% AUs)

DO_inst_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_YearRound_instant_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

DO_estuary_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_estuary_instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

DO_estuary_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/DO/Data_Review/DO_Estuary_Yearround_IR_data_ALLDATA.csv",
                                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

pH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/pH/Data_Review/pH_IR_data_ALLDATA.csv",
               stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

temp <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Temperature/Data_Review/Temperature_IR_data_ALLDATA - final.csv",
                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  mutate(Spawn_criteria = ifelse(Spawn_type == "Spawn", 13, "" ) ) %>%
  filter(AU_ID %in% AUs)

temp <- temp[,c(1:12, 50, 13:49)]

Tox_AL_Ammonia <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Ammonia_IR_Data_ALLDATA.csv",
                           stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

Tox_AL_CU <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Cu_IR_Data_ALLDATA.csv",
                      stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

Tox_AL_Hardness_Metals <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Hardness_Metals_IR_Data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

Tox_AL_Others <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Others_IR_Data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

Tox_AL_Penta <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_AL/Data_Review/TOX_AL_Pentachlorophenol_IR_data_ALLDATA.csv",
                         stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

Tox_HH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_HH/Data_Review/Tox_HH_IR_data_ALLDATA.csv",
                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)

Tox_HH_Hg_tissue <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Tox_HH/Data_Review/Tox_HH_hg_tissue_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  filter(AU_ID %in% AUs)




# 


bacteria_coast_contact_basin <-  bacteria_coast_contact




bacteria_fresh_contact_basin <-   bacteria_fresh_contact 

bacteria_Shell_harvest_basin <-   bacteria_Shell_harvest 


wb <- createWorkbook()
addWorksheet(wb, "E coli")
addWorksheet(wb, "Enterococcus")
addWorksheet(wb, "Fecal Coliform")

writeData(wb,"E coli",  bacteria_fresh_contact_basin, rowNames = FALSE)
writeData(wb,"Enterococcus", bacteria_coast_contact_basin, rowNames = FALSE)
writeData(wb,"Fecal Coliform", bacteria_Shell_harvest_basin, rowNames = FALSE)

saveWorkbook(wb, paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_Bacteria.xlsx"), 
             overwrite = TRUE)


chl_basin <-   chl

if(exists('chl_basin')){
  write.xlsx( chl_basin, 
              file = paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_Chlorophyll.xlsx"), 
              overwrite = TRUE)
  
}


DO_cont_spawn_basin <-   DO_cont_spawn 

DO_cont_yearround_basin <-   DO_cont_yearround

DO_instant_spawn_basin <-   DO_instant_spawn

DO_inst_yearround_basin <-   DO_inst_yearround %>%
  filter(OWRD_Basin == basin)

DO_estuary_spawn_basin <-   DO_estuary_spawn

DO_estuary_yearround_basin <-   DO_estuary_yearround 

pH_basin <-   pH 

if(exists('pH_basin')){
  write.xlsx( pH_basin, 
              file = paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_pH.xlsx"), 
              overwrite = TRUE)
  
}


temp_basin <-   temp

if(exists('temp_basin')){
  write.xlsx( temp_basin, 
              file = paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_Temperature.xlsx"), 
              overwrite = TRUE)
  
}

Tox_AL_Ammonia_basin <-   Tox_AL_Ammonia 

Tox_AL_CU_basin <-   Tox_AL_CU 

Tox_AL_Hardness_Metals_basin <-   Tox_AL_Hardness_Metals 

Tox_AL_Others_basin <-   Tox_AL_Others 

Tox_AL_Penta_basin <-   Tox_AL_Penta 

Tox_HH_basin <-   Tox_HH 

Tox_HH_Hg_tissue_basin <-   Tox_HH_Hg_tissue 


wb <- createWorkbook()
addWorksheet(wb, "DO_spawn_continuous")
addWorksheet(wb, "DO_spawn_instantaneous")

writeData(wb,"DO_spawn_continuous",  DO_cont_spawn_basin, rowNames = FALSE)
writeData(wb,"DO_spawn_instantaneous", DO_instant_spawn_basin, rowNames = FALSE)

saveWorkbook(wb, paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_DO_Spawning.xlsx"), 
             overwrite = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "DO_yearround_continuous")
addWorksheet(wb, "DO_yearround_instantaneous")

writeData(wb,"DO_yearround_continuous",  DO_cont_yearround_basin, rowNames = FALSE)
writeData(wb,"DO_yearround_instantaneous", DO_inst_yearround_basin, rowNames = FALSE)

saveWorkbook(wb, paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_DO_Yearround.xlsx"), 
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


saveWorkbook(wb, paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_Aquatic_Life_Toxics.xlsx"), 
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


saveWorkbook(wb, paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/2018_2020 Draft IR Data/", name,"/", name, "_Human_Health_Toxics.xlsx"), 
             overwrite = TRUE)





