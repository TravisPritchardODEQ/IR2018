# This code generates temperature assessment results and temperature data
# in response to a request from Weyerhaeuser
# The provided a list of HUC12 and wanted watershed units from those HUCs
# Saving this script due to the function to extract HUC12 from WS AUs
# and combining the assessed data with station information




library(tidyverse)
library(openxlsx)
library(AWQMSdata)
library(stringi)




load("E:/Documents/2018-2020_IR_Database/data/assessment_display.Rdata")

WH_request <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/Weyerhaeuser/WY HUC12 Watersheds List_DEQ Request .xlsx")  %>%
  mutate(HUC12.ID = as.character(HUC12.ID))


temp_assessments <- joined_BU_summary %>%
  filter(Char_Name == 'Temperature') %>%
  mutate(huc12 = sapply(strsplit(as.character(AU_ID), "_"), function(x) x[[3]][1])) %>%
  filter(huc12 %in% WH_request$HUC12.ID)


temp_assessments <- temp_assessments[,c(1:2,11,3:10)]


write.xlsx(temp_assessments, '//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/Weyerhaeuser/WH_temp_assessments.xlsx')


temperature_data_final <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Temperature/Data_Review/Temperature_IR_data_ALLDATA - final.csv",
                                   stringsAsFactors = FALSE)

WH_temperature_data <- temperature_data_final %>%
  filter(AU_ID %in% temp_assessments$AU_ID) %>%
  arrange(AU_ID)

wH_stations <- query_stations(mlocs = WH_temperature_data$MLocID) %>%
  select(MLocID, StationDes, Lat_DD, Long_DD, Datum)


WH_temperature_data_stations <- WH_temperature_data %>%
  left_join(wH_stations, by = "MLocID")


WH_temperature_data_stations <- WH_temperature_data_stations[,c(1:2, 51:54, 3:49)] 


write.xlsx(WH_temperature_data_stations, '//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/Weyerhaeuser/WH_temp_data.xlsx')
