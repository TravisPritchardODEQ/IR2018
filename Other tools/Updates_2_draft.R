library(openxlsx)
library(readxl)
library(tidyverse)
load("ATTAINS/LU_Pollutant.Rdata")
path <- "W:\\2018IRFiles\\2018_WQAssessment\\Draft List\\Updates to draft list\\Eugene_R_files\\"

### Pull in draft list####
Old_Draft = read_excel("W:\\2018IRFiles\\2018_WQAssessment\\Draft List\\Rollup\\Basin_categories\\ALL BASINS_categories.xlsx", sheet = "IR Categories") %>%
  rename(IR_category_Original=IR_category) %>%
  select(-Char_Name,
         -OWRD_Basin,
         -Data_Review_Code,
         -Data_Review_Comment,
         -Rational)
#### Bacteria #####
bac_new <- read.csv(paste0(path, "\\Bacteria_Fresh_Contact_IR_Categories_ALLDATA.csv")) %>%
  mutate(Pollu_ID = '76',
         Char_Name = "E. coli",
         WQstd_code = "1",
         Rational = as.character(Rational)) %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         IR_category,
         Rational) %>%
  rename(IR_category_Updated =IR_category)


### AL Ammonia ####
AL_ammon_new <- read.csv(paste0(path, "\\TOX_AL_Ammonia_IR_Categories_ALLDATA.csv")) %>%
  mutate(Pollu_ID = '6',
         WQstd_code = "15",
         Rational = as.character(Rational)) %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         IR_category,
         Rational) %>%
  rename(IR_category_Updated =IR_category)
### AL others ####
AL_tox_new <- read.csv(paste0(path, "\\TOX_AL_Others_IR_Categories_ALLDATA.csv")) %>%
  mutate(Pollu_ID = "",
         WQstd_code = "15",
         Char_Name = as.character(Char_Name),
         Char_Name = case_when(Char_Name == "Alkalinity, total" ~ 'Alkalinity',
                               #Char_Name == "Alkalinity, bicarbonate" ~ 'Alkalinity',
                               Char_Name == "PCBs"  ~ 'Polychlorinated Biphenyls (PCBs)',
                               Char_Name == 'DDT' ~ "DDT 4,4'",
                               Char_Name == 'Lindane' ~ 'BHC Gamma (Lindane)',
                               TRUE ~ Char_Name)) %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         IR_category,
         Rational) %>%
  left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
  mutate(Pollu_ID = LU_Pollu_ID) %>%
  select(-LU_Pollu_ID) %>%
  rename(IR_category_Updated =IR_category)

#### AL copper ####
AL_cop_new <- read.csv(paste0(path, "\\Cu_BLM_results.csv")) %>%
  mutate(Pollu_ID = '45',
         Char_Name = "Copper",
         WQstd_code = "15") %>%
  select(AU_ID,
         WQstd_code,
         Char_Name,
         Pollu_ID,
         IR_Category,
         Rational) %>%
   rename(IR_category_Updated =IR_Category)



#### AL hardnes metals #####
AL_metals_new <- read.csv(paste0(path, "\\TOX_AL_Hardness_Metals_IR_Categories_ALLDATA.csv")) %>%
  mutate(Pollu_ID = "",
         WQstd_code = "15") %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         IR_category,
         Rational) %>%
  left_join(Pollu_IDs, by = c('Char_Name' = 'LU_Pollutant')) %>%
  mutate(Pollu_ID = LU_Pollu_ID) %>%
  select(-LU_Pollu_ID) %>%
  rename(IR_category_Updated =IR_category)

#### Human Health ####
HH_new <- read.csv(paste0(path, "\\Tox_HH_IR_Categories_ALLDATA.csv"),stringsAsFactors = FALSE) %>% 
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = "16") %>%
  group_by(AU_ID, Char_Name) %>%
  mutate(match = ifelse(Simplified_sample_fraction == Crit_Fraction, 1, 0 ),
         keep = ifelse(max(match, na.rm = TRUE) == 1 & match == 1 |
                         max(match, na.rm = TRUE) == 0 & match == 0, 1, 0)) %>%
  filter(keep == 1) %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         IR_category,
         Rational) %>%
  distinct() %>%
  rename(IR_category_Updated =IR_category)

##### pH #####
pH_new <- read.csv(paste0(path, "pH_IR_categories_ALLDATA.csv")) %>% 
mutate(Pollu_ID = '124',
       Char_Name = "pH",
       WQstd_code = "10") %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID, 
         WQstd_code,
         IR_category,
         Rational) %>%
  rename(IR_category_Updated =IR_category)

#### DO ####
DO_C_YR_new <- read.csv(paste0(path, "DO_yearround_continuous_IR_categories_ALLDATA.csv")) %>% 
  mutate(Pollu_ID = '154',
         Char_Name = "Dissolved Oxygen",
         WQstd_code = "3",
         Period = "Year Round") %>%
  group_by(AU_ID) %>%
  mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
  ungroup() %>%
  mutate(Char_Name = ifelse(mult_flag == 1, paste0(Char_Name, "- ", DO_Class), Char_Name )) %>%
  select(AU_ID,
         Period,
         Char_Name,
         WQstd_code,
         IR_category,
         Pollu_ID,
         Rational) %>%
  rename(IR_category_Updated =IR_category)

DO_C_Spawn_new <- read.csv(paste0(path, "DO_Spawning_continuous_IR_categories_ALLDATA.csv")) %>% 
mutate(Pollu_ID = "154",
       WQstd_code = "3",
       Char_Name = "Dissolved Oxygen",
       Period = "Spawning") %>%
   select(AU_ID,
         Period,
         Char_Name,
         WQstd_code,
         Pollu_ID,
         IR_category,
         Rational) %>%
  rename(IR_category_Updated =IR_category)

DO_I_YR_new <- read.csv(paste0(path, "DO_yearround_instantaneous_IR_categories_ALLDATA.csv")) %>% 
  mutate(Pollu_ID = "154",
         WQstd_code = "3",
         Char_Name = "Dissolved Oxygen",
         Period = "Year Round") %>%
  group_by(AU_ID) %>%
  mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
  ungroup() %>%
  mutate(Char_Name = ifelse(mult_flag == 1, paste0(Char_Name, "- ", DO_Class), Char_Name )) %>%
  select(AU_ID,
         Period,
         Char_Name,
         WQstd_code,
         Pollu_ID,
         IR_category,
         Rational) %>%
  rename(IR_category_Updated =IR_category)

DO_I_Spawn_new <- read.csv(paste0(path, "DO_Spawning_instantaneous_IR_categories_ALLDATA.csv")) %>% 
  mutate(Pollu_ID = "154",
         WQstd_code = "3",
         Char_Name = "Dissolved Oxygen",
         Period = "Spawning") %>%
  group_by(AU_ID) %>%
  mutate(mult_flag = ifelse(n() > 1, 1, 0)) %>%
  ungroup() %>%
  mutate(Char_Name = ifelse(mult_flag == 1, paste0(Char_Name, "- ", DO_Class), Char_Name )) %>%
  select(AU_ID,
         Period,
         Char_Name,
         WQstd_code,
         Pollu_ID,
         IR_category,
         Rational) %>%
  rename(IR_category_Updated =IR_category)

         
#### all ####
all <- bind_rows(bac_new,AL_ammon_new,AL_tox_new,AL_cop_new,AL_metals_new,pH_new,HH_new) %>%
  mutate(Period = "Year Round")
Updates <- all %>%
  left_join(Old_Draft, by = c("AU_ID", "Pollu_ID", "WQstd_code")) %>%
  select(AU_ID,Pollu_ID,WQstd_code,Char_Name,Period.y,IR_category_Original,IR_category_Updated,Rational) %>%
  rename(Period=Period.y)

all_DO <- bind_rows(DO_I_Spawn_new,DO_I_YR_new,DO_C_Spawn_new,DO_C_YR_new)
Updates_DO <- all_DO %>%
  left_join(Old_Draft, by = c("AU_ID", "Pollu_ID", "WQstd_code","Period")) %>%
  select(AU_ID,Pollu_ID,WQstd_code,Char_Name,Period,IR_category_Original,IR_category_Updated,Rational) 

Updates_all<-bind_rows(Updates,Updates_DO)
write.csv(Updates_all,(paste0(path,"\\Eugene_compare.csv")))
