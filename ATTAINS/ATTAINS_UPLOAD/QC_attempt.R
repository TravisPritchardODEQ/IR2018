### ATTAINS QC ### 

# currently in ATTAINS 

ap <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/ATTAINS_DOWNLOAD/april_14_2020/parameters.csv") %>%
      mutate(QC_test = 1)
au <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/ATTAINS_DOWNLOAD/april_14_2020/uses.csv")

# missing from attains 
a_miss <- Param_upload %>% 
          left_join(ap, by = c('ASSESSMENT_UNIT_ID','PARAM_NAME','PARAM_USE_NAME','PARAM_STATUS_NAME')) %>% filter(is.na(QC_test)) #,'PARAM_ATTAINMENT_CODE')) %>% #,   
                               #'PARAM_AGENCY_CODE','PARAM_POLLUTANT_INDICATOR','PARAM_YEAR_LISTED','PARAM_PRIORITY_RANKING','PARAM_COMMENT',           
                               #'PARAM_DELISTING_REASON','PARAM_DELISTING_COMMENT'# %>%
          

deq_miss <- ap %>% 
  left_join(Param_upload, by = c('ASSESSMENT_UNIT_ID','PARAM_NAME','PARAM_USE_NAME',#'PARAM_STATUS_NAME','PARAM_ATTAINMENT_CODE')) %>% #,   
  #'PARAM_AGENCY_CODE','PARAM_POLLUTANT_INDICATOR','PARAM_YEAR_LISTED','PARAM_PRIORITY_RANKING','PARAM_COMMENT',           
  #'PARAM_DELISTING_REASON','PARAM_DELISTING_COMMENT')) %>%
  filter(is.na(OWRD_Basin))
