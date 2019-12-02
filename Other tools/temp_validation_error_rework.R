library(tidyverse)


#This script takes in the reassessed temperature data and works them into the rest of the temperature assessments.


#read in new temp assessments

new_temp_assessments <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Temperature/Data_Review/10-21-2019 temp rerun - validation error/Temperature_IR_categorization_ALLDATA - no cat3.csv",
                                 stringsAsFactors = FALSE)


reassessed_AUs <- unique(new_temp_assessments$AU_ID)


Basins <- unique(new_temp_assessments$OWRD_Basin)

Basins2 %in% Basins

#loop through each basin folder and get the assessed data. Write some preliminary tables

for (i in 1:length(Basins)) {
  
  
  basin <- Basins[i]
  
  
  
  print(paste("Starting basin:",basin ))
  
  
  new_temp_assessments_basin <- new_temp_assessments %>%
    filter(OWRD_Basin == basin)
  
  basin_AU <- unique(new_temp_assessments_basin$AU_ID)
  
  temp <- read.csv(paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                          basin,
                          "/",
                          'Temperature_IR_categorization_',basin, '.csv'), stringsAsFactors = FALSE) %>%
    filter(!AU_ID %in% basin_AU) %>%
    bind_rows(new_temp_assessments_basin) 
  
  write.csv(temp, file = paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Completed_IR_team_Review/', 
                                 basin,
                                 "/",
                                 'Temperature_IR_categorization_',basin, '_with_validation_error_fix.csv'),
            row.names = FALSE)
    
  
  
  
  
  
  }