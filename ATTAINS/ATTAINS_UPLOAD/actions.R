#### Updating Actions #### 

au <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/actions/action_AU.csv")

a_pollu <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/actions/action_pollu.csv") 

a_pp <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/actions/pollu_Param.csv")


au_pollu <- au %>% 
            left_join(a_pollu, by = c('ACTION_ID'))


au_pp <- au %>% 
  left_join(a_pp, by = c('ACTION_ID')) %>%
 

# get associated actions 
a_param <- au_pp %>%
           select(ACTION_ID,AU_ID,PARAMETER_NAME) %>% 
           distinct() 
           
aa <- Param_upload %>%
      filter(PARAM_STATUS_NAME == 'Cause') %>%
      select(ASSESSMENT_UNIT_ID,PARAM_NAME) %>%
      distinct() %>% 
      left_join(a_param, c('ASSESSMENT_UNIT_ID'= 'AU_ID','PARAM_NAME'= 'PARAMETER_NAME')) %>%
      filter(!is.na(ACTION_ID)) %>%
      mutate(ACTION_TYPE = "TMDL")


write.csv(au_pollu,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/actions/au_pollu.csv")
write.csv(au_pp,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/actions/au_pp.csv")                     
write.csv(aa,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/ATTAINS_Uploads/actions/aa.csv") 
