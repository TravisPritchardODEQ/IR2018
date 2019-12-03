library(tidyverse)
library(openxlsx)
library(lubridate)


tmp_xwalk_season <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/temp_xwalk_season.csv",
                             stringsAsFactors = FALSE) %>%
  group_by(AU_ID, Time_Period) %>%
  arrange(AU_ID, Time_Period) %>%
  mutate(keep = ifelse(ASSESSME_1 == max(ASSESSME_1), 1, 0 )) %>%
  filter(keep == 1) %>%
  distinct(AU_ID, Time_Period, .keep_all = TRUE) %>%
  select(-keep) 

write.csv(tmp_xwalk_season, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/temp_xwalk_season_unique.csv",
          row.names = FALSE)

temp_list_2012 <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/OR_Streams_WQ_2012_v4.csv",
                      stringsAsFactors = FALSE) %>%
  filter(STATUS_ID %in% c(14,15,16,17,20),
         Pollu_ID == 132)


tmp_xwalk_season_unaltered <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/temp_xwalk_season.csv",
                             stringsAsFactors = FALSE) 

missing_recordiDs <- temp_list_2012 %>%
  filter(!RECORD_ID %in% tmp_xwalk_season_unaltered$RECORD_ID)

write.csv(missing_recordiDs, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/Final_Files/missing_temp_listings.csv",
          row.names = FALSE)