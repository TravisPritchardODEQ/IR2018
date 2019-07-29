library(rgdal)
library(openxlsx)
library(tidyverse)
library(data.table)

options(scipen = 99999999)

# Copy the oregon standards out of the geodatabase ------------------------

# path <- '//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Standards/GeoRef_Standards.gdb'
#
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(path)
#
# oregon_standards <- readOGR(dsn=path,layer="Oregon_Standards")
# 
# rm(oregon_standards)
# 
# standards <- as.data.frame(oregon_standards)
# 
# standards <- standards %>%
#   mutate_if(is.factor, as.character)
# save(standards, file = "Other tools/Standard_layer_display/standards.Rdata")
# 
load("Other tools/Standard_layer_display/standards.Rdata")


# Get relationships -------------------------------------------------------


BenUse_codes <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'BenUse_codes')

# BenUse_codes_flat <- BenUse_codes %>%
#   group_by(ben_use_code) %>%
#   summarise(Designated_uses =  str_c(str_sort(ben_use), collapse = "; "))

WQStds_codes <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'WQStds_codes')
wqstds_by_useID <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'wqstds_by_useID')

wqstrd_combined <- wqstds_by_useID %>%
  left_join(WQStds_codes) %>%
  mutate(WQstd_OAR = case_when(!is.na(OAR_Criteria) ~ paste0(wqstd, " (OAR: ", OAR_Criteria, ")"),
                               TRUE ~ wqstd
                               )
  )


ben_use_wqstrd <- BenUse_codes %>%
  left_join(wqstrd_combined)

ben_use_wqstrd_collapsed <- ben_use_wqstrd %>%
  group_by(ben_use_code) %>%
  summarise(Designated_uses =  str_c(str_sort(unique(ben_use)), collapse = "; "),
            applicable_strds = str_c(str_sort(unique(WQstd_OAR)), collapse = "; "))


fwrite(ben_use_wqstrd_collapsed, file = "Other tools/Standard_layer_display/ben_use_wqstrd.csv",
       row.names = FALSE)

standards_ben_use <- standards %>%
  left_join(ben_use_wqstrd_collapsed) %>%
  mutate(GNIS_Name = ifelse(is.na(GNIS_Name), "Unnamed Stream", GNIS_Name ))


temp_criteria <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'temperature_criteria') %>%
  mutate(TempCode = as.character(TempCode)) %>%
  rename(`Temperature Criterion (7dADM-°C)` = `Criterion.(7dADM-°C)`
)

temp_spawning_dates <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'Spawning_dates') %>%
  mutate(SpawnCode = as.character(SpawnCode),
         DO_SpawnCode = as.character(DO_SpawnCode)) %>%
  select(-DO_SpawnCode) %>%
  rename(Temperature_Spawn_dates = Spawn_date_range)

DO_spawning_dates <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'Spawning_dates') %>%
  mutate(SpawnCode = as.character(SpawnCode),
         DO_SpawnCode = as.character(DO_SpawnCode)) %>%
  select(-SpawnCode) %>%
  rename(DO_Spawn_dates = Spawn_date_range)

DO_criteria <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'DO_criteria') %>%
  mutate(DO_code = as.character(DO_code)) %>%
  mutate(DO_code = ifelse(nchar(DO_code) == 1, paste0("0", DO_code), DO_code ))

pH_criteria <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'pH_criteria') %>%
  mutate(pH_code = as.character(pH_code)) %>%
  mutate(pH_code = ifelse(nchar(pH_code) == 1, paste0("0", pH_code), pH_code ))

ALtoxics_criteria <-  read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'ALtoxics_criteria') %>%
  mutate(WaterType_code = as.character(WaterType_code)) %>%
  rename(WaterTypeCode = WaterType_code) %>%
  mutate(WaterTypeCode = ifelse(nchar(WaterTypeCode) == 1, paste0("0", WaterTypeCode), WaterTypeCode ))

Bacteria_criteria <- read.xlsx("Other tools/Standard_layer_display/BenUse_standards_relation_tables.xlsx", sheet = 'Bacteria_criteria') %>%
  mutate(bacteria_code = as.character(bacteria_code)) %>%
  rename(BacteriaCode = bacteria_code) %>%
  mutate(BacteriaCode = ifelse(nchar(BacteriaCode) == 1, paste0("0", BacteriaCode), BacteriaCode )) %>%
  rename(Bacteria_criteria = bacteria_criteria,
         Bacteria_indicator = bacteria_indicator)

standards_ben_use_all <- standards_ben_use %>%
  left_join(temp_criteria, by = "TempCode") %>%
  left_join(temp_spawning_dates, by = "SpawnCode") %>%
  left_join(DO_spawning_dates, by = "DO_SpawnCode") %>%
  left_join(DO_criteria, by = "DO_code") %>%
  left_join(pH_criteria,by = "pH_code") %>%
  left_join(ALtoxics_criteria,  by = "WaterTypeCode") %>%
  left_join(Bacteria_criteria, by = "BacteriaCode")


standards_ben_use_all_smaller <- standards_ben_use_all[,c(4,5,7,15,16,30:41)]
# write.csv(standards_ben_use_all, file = "Other tools/Standard_layer_display/standards_layer_for_display.csv",
#           row.names = FALSE)

fwrite(standards_ben_use_all, file = "Other tools/Standard_layer_display/standards_layer_for_display.csv",
        row.names = FALSE)

#save(standards_ben_use_all, file = "Other tools/Standard_layer_display/standards_ben_use_all.Rdata")

load("Other tools/Standard_layer_display/standards_ben_use_all.Rdata")
