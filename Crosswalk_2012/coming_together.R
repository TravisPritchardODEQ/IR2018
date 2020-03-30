#require(RODBC)
library(tidyverse)
library(IRlibrary)
library(openxlsx)
library(readxl)
library(data.table)

#disable scientific notation
options(scipen=999)



#2012 list - add to IR database and git???
old_list_v4 <- read.csv("Crosswalk_2012/OR_Streams_WQ_2012_v4.csv")
old_list_v4 <- filter(old_list_v4, STATUS_ID %in% c(14,15,16,17,20)) %>%
  select("RECORD_ID","POLLUTANT","Pollu_ID")
old_list <- read.csv("Crosswalk_2012/OR_Streams_WQ_2012_v6.csv")
old_list_lakes <- read.csv("Crosswalk_2012/OR_Lakes_WQ_2012.csv")
impair_old_list <- filter(old_list, STATUS_ID %in% c(14,15,16,17,20)) %>% 
  select("ASSESSMENT","RECORD_ID","STREAM_LAK","STREAM_NAM","LAKE_NAME","BEACH_NAME",
         "LLID_STR_1","LLID_LAKE","SEGMENT_ID","HUC_4TH_NA","HUC_3RD_NA","MILES","RM1","RM2","POLLUTANT","SEASON",
         "SEASON_ID","ASSESSME_1","ASSESSME_2","ACTION","ACTION_ID", 
         "NUMERIC_CR","AFFECTED_U","LISTING_ST","STATUS_ID","PREVIOUS_S","PREVIOUS_A","LISTING_YE","TMDL_INFO",
         "CRITERIA","COMMENTS","SUMMARY")
impair_old_list_lakes <- filter(old_list_lakes, STATUS_ID %in% c(14,15,16,17,20))
lakes <-  impair_old_list_lakes %>%
  filter(!RECORD_ID %in% impair_old_list$RECORD_ID) %>%
  select("ASSESSMENT","RECORD_ID","STREAM_LAK","STREAM_NAM","LAKE_NAME","BEACH_NAME",
         "LLID_STR_1","LLID_LAKE","SEGMENT_ID","HUC_4TH_NA","HUC_3RD_NA","MILES","RM1","RM2","POLLUTANT","SEASON",
         "SEASON_ID","ASSESSME_1","ASSESSME_2","ACTION","ACTION_ID", 
         "NUMERIC_CR","AFFECTED_U","LISTING_ST","STATUS_ID","PREVIOUS_S","PREVIOUS_A","LISTING_YE","TMDL_INFO",
         "CRITERIA","COMMENTS","SUMMARY")
impaired_2012 <- rbind(impair_old_list,lakes) %>%
  left_join(old_list_v4,by = c("RECORD_ID","POLLUTANT")) %>%
  select("ASSESSMENT","RECORD_ID","Pollu_ID","STREAM_LAK","STREAM_NAM","LAKE_NAME","BEACH_NAME",
         "LLID_STR_1","LLID_LAKE","SEGMENT_ID","HUC_4TH_NA","HUC_3RD_NA","MILES","RM1","RM2","POLLUTANT","SEASON",
       "SEASON_ID","ASSESSME_1","ASSESSME_2","ACTION","ACTION_ID", 
       "NUMERIC_CR","AFFECTED_U","LISTING_ST","STATUS_ID","PREVIOUS_S","PREVIOUS_A","LISTING_YE","TMDL_INFO",
       "CRITERIA","COMMENTS","SUMMARY")
write.csv(impaired_2012,"Crosswalk_2012/impaired_2012_LLID.csv")

## moved to excel to add Pollu_Id where missing and add
##### put above on a seperate file???
#
#impaired_2012 <- read.csv("impaired_2012.csv")

#Function to interpolate crosswalk method
fun_method <- function(df){
   df2 <- df %>%
    group_by(RECORD_ID) %>%
    mutate(Method = max(Method, na.rm = TRUE))
  
  return(df2)
  
}


# crosswalk files - do these go in git folder? 
DO <- fun_method(read_excel("Crosswalk_2012/Crosswalk_DissolvedOxygen2.xlsx", sheet = "Crosswalk"))
algae <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Algae_Weeds.xlsx", sheet = "Crosswalk"))
Hg <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Mercury.xlsx", sheet = "Crosswalk"))
P <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Phosporus.xlsx", sheet = "Crosswalk"))
pH <- fun_method(read_excel("Crosswalk_2012/Crosswalk_pH.xlsx", sheet = "Crosswalk"))
DDTs <- fun_method(read_excel("Crosswalk_2012/Crosswalk_DDTs.xlsx", sheet = "Crosswalk"))
fecal <- fun_method(read_excel("Crosswalk_2012/Crosswalk_fecal.xlsx", sheet = "Crosswalk"))
sediment <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Sedimentation.xlsx", sheet = "Crosswalk"))
entero <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Entero.xlsx", sheet = "crosswalk"))
tub_TDG <- fun_method(read_excel("Crosswalk_2012/Crosswalk_TurbidityandTDG.xlsx", sheet = "Crosswalk"))
ecoli <- fun_method(read_excel("Crosswalk_2012/Crosswalk_ecoli.xlsx", sheet = "Crosswalk"))
chla <- fun_method(read_excel("Crosswalk_2012/Crosswalk_chla.xlsx", sheet = "Crosswalk"))
bio <- fun_method(read_excel("Crosswalk_2012/Crosswalk_BioCriteria.xlsx", sheet = "Crosswalk"))
toxics <- fun_method(read_excel("Crosswalk_2012/Crosswalk_MostToxics.xlsx", sheet = "Crosswalk"))
metals <- fun_method(read_excel("Crosswalk_2012/Crosswalk_metals.xlsx", sheet = "Crosswalk"))
temp <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Temperature.xlsx", sheet = "Crosswalk2"))
flow <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Flow.xlsx", sheet = "Crosswalk"))
hab <- fun_method(read_excel("Crosswalk_2012/Crosswalk_Habitat.xlsx", sheet = "Crosswalk"))


#### bring in station data
DO_S <- read_excel("Crosswalk_2012/Crosswalk_DissolvedOxygen2.xlsx", sheet = "Station")
algae_S <- read_excel("Crosswalk_2012/Crosswalk_Algae_Weeds.xlsx", sheet = "Station")
Hg_S <- read_excel("Crosswalk_2012/Crosswalk_Mercury.xlsx", sheet = "Station")
P_S <- read_excel("Crosswalk_2012/Crosswalk_Phosporus.xlsx", sheet = "Station")
pH_S <- read_excel("Crosswalk_2012/Crosswalk_pH.xlsx", sheet = "Station")
DDTs_S <- read_excel("Crosswalk_2012/Crosswalk_DDTs.xlsx", sheet = "Station")
fecal_S <- read_excel("Crosswalk_2012/Crosswalk_fecal.xlsx", sheet = "Station")
sediment_S <- read_excel("Crosswalk_2012/Crosswalk_Sedimentation.xlsx", sheet = "Station")
entero_S <- read_excel("Crosswalk_2012/Crosswalk_Entero.xlsx", sheet = "Station")
tub_TDG_S <- read_excel("Crosswalk_2012/Crosswalk_TurbidityandTDG.xlsx", sheet = "Station")
ecoli_S <- read_excel("Crosswalk_2012/Crosswalk_ecoli.xlsx", sheet = "Station")
chla_S <- read_excel("Crosswalk_2012/Crosswalk_chla.xlsx", sheet = "Station")
bio_s <- read_excel("Crosswalk_2012/Crosswalk_BioCriteria.xlsx", sheet = "Station")
toxics_S <- read_excel("Crosswalk_2012/Crosswalk_MostToxics.xlsx", sheet = "Station")
metals_S <- read_excel("Crosswalk_2012/Crosswalk_metals.xlsx", sheet = "Station")
temp_S <- read_excel("Crosswalk_2012/Crosswalk_Temperature.xlsx", sheet = "Station")



x_walk_all <- rbind(DO,algae,Hg,P,pH,DDTs,fecal, sediment,entero,tub_TDG,ecoli,chla,bio,toxics,metals,temp,flow,hab) %>%
  mutate(AU_Category = ifelse(is.na(AU_Category), "Unassessed", AU_Category )) %>%
  mutate(AU_Category = ifelse(AU_Category %in% c("Not Assessed",
                                                 "Unassessed",
                                                 "unassessed",
                                                 "Not assessed"), "Unassessed", AU_Category )) %>%
  mutate(Period = case_when(Period %in% c("Spawn",
                                          "spawn") ~ 'Spawning',
                            Period %in% c("YR",
                                          "yr") ~ "YearRound",
                            TRUE ~ "YearRound")) %>%
  mutate(Pollutant = ifelse(Pollu_ID == 154, 'Dissolved Oxygen', Pollutant )) %>%
  filter(!is.na(Pollutant))

impaired_2012_2 <- impaired_2012 %>%
  mutate(Period = case_when(Pollu_ID == 154 & SEASON_ID %in% c(1,2,3,16,71,101) ~ "YearRound",
                            Pollu_ID == 154 & SEASON_ID %in% c(8,23,30,31,38,39,41,42,45,46,49,50,51,100) ~ "Spawning",
                            Pollu_ID == 132 & SEASON_ID %in% c(1,2,3,16,32,71,72,101) ~ "YearRound",
                            Pollu_ID == 132 & SEASON_ID %in% c(6,8,9,15,27,30,31,39,40,41,42,45,46,48,49,50,51,64,100) ~ "Spawning",
                            !Pollu_ID %in% c(154,132) ~ "YearRound"))


X_walk_methods <- x_walk_all %>%
  left_join(impaired_2012_2, by = c("RECORD_ID","Pollu_ID", 'Period')) %>%
  select(RECORD_ID,AU_ID, Pollutant, Pollu_ID, Period, AU_Category, Method) %>%
  filter(!is.na(RECORD_ID),
         !is.na(AU_ID)) %>%
  arrange(RECORD_ID)

write.xlsx(X_walk_methods, file = "Crosswalk_2012/xwalk_method.xlsx")

x_walk_impaired <- x_walk_all %>% 
  filter(AU_Category %in% c('Category 5','4C')) %>%
  select(RECORD_ID,Pollu_ID,AU_ID) %>%
  left_join(impaired_2012, by = c("RECORD_ID","Pollu_ID", "Period")) %>%
  mutate(Period = case_when(Pollu_ID == 154 & SEASON_ID %in% c(1,2,3,16,71,101) ~ "YearRound",
                            Pollu_ID == 154 & SEASON_ID %in% c(8,23,30,31,38,39,41,42,45,46,49,50,51,100) ~ "Spawning",
                            Pollu_ID == 132 & SEASON_ID %in% c(1,2,3,16,32,71,72,101) ~ "YearRound",
                            Pollu_ID == 132 & SEASON_ID %in% c(6,8,9,15,27,30,31,39,40,41,42,45,46,48,49,50,51,64,100) ~ "Spawning",
                            !Pollu_ID %in% c(154,132) ~ "YearRound")) %>%
  filter(!is.na(RECORD_ID))
 

save(x_walk_impaired, file = 'ATTAINS/x_walk_impaired.Rdata')
write.csv(x_walk_impaired, "x_walk_impaired_LLID.csv", row.names = FALSE)

# some parameters have no station data
xwalk_station <- bind_rows(DO_S,Hg_S,P_S,pH_S,DDTs_S,fecal_S,entero_S,ecoli_S,chla_S,bio_s,toxics_S,metals_S,temp_S)
x_walk_impaired_station <- xwalk_station %>% 
  left_join(x_walk_impaired, by = c("RECORD_ID","Pollu_ID"))


write.xlsx(xwalk_station, file = "Crosswalk_2012/xwalk_stations.xlsx")
write.csv(x_walk_impaired_station, "x_walk_impaired_station.csv", row.names = FALSE)

### crosswalk vision file from Dwane Young 1/15/2020

missing_v <- read.csv("Missing_Vision.csv") %>%
  left_join(x_walk_impaired, by = c("Pollu_ID" = "Pollu_ID","LLID"= "LLID_STR_1"))

write.csv(missing_v, "missing_v_AU_ID.csv")
