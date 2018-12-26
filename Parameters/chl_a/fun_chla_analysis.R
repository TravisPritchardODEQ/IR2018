library(tidyverse)
library(lubridate)
library(zoo)




chl_assessment <- function(df){

chla_data <- df %>%
  #filter(!is.na(AU_ID)) %>%
  mutate( month = month(SampleStartDate),
          yrfromstart = year(SampleStartDate) - 2008,
          monthfromstart = month + 12*yrfromstart) 

chla_mo_avg <- chla_data %>%
  arrange(AU_ID,monthfromstart ) %>%
  group_by(AU_ID, monthfromstart, Chla_Criteria, OWRD_Basin, Char_Name) %>%
  summarise(monthaverage = mean(IRResultNWQSunit))
  




# Figure out where we have 3 consecutive monthly averages -----------------


chla_consec_mon <- chla_mo_avg %>%
  group_by(AU_ID, Chla_Criteria) %>%
  arrange(AU_ID, monthfromstart, Chla_Criteria) %>%
  #calculate difference in months from previous result
  mutate(diffs = c(0, diff(monthfromstart)))#%>%
  #mutate(diffs = ifelse(diffs > 1, 9999, 1 ))





chla_avgs <- chla_consec_mon %>%
  # flag if there are two consecutive diffs = 1, which indicates 3 consecutive months
  mutate(consecutive3 = ifelse((lag(diffs , 1) == 1)  & diffs == 1, 1, 0 ) ) %>%
  group_by(AU_ID, Chla_Criteria) %>%
  mutate(avg.3.mo = ifelse(consecutive3 == 1, rollmean(monthaverage,
                                                       3,
                                                       align = 'right',
                                                       fill= NA,
                                                       na.rm = T), NA ) )

chla_data_analysis <-  chla_data %>%
  left_join(chla_avgs, by = c("AU_ID", "monthfromstart", "Chla_Criteria", "OWRD_Basin", "Char_Name")) %>%
  select(-diffs, -consecutive3) %>%
  arrange(AU_ID, monthfromstart)

IR_export(chla_data_analysis, "Parameters/chl_a/Data_Review", "Chla", "data" )


chl_categories <- chla_data_analysis %>%
  group_by(AU_ID, MonLocType, OWRD_Basin, Char_Name, Chla_Criteria) %>%
  summarise(max_result = max(Result_cen),
            max_mo_avg = max(monthaverage),
            max_3_mo_avg = max(avg.3.mo)) %>%
  mutate(IR_category = ifelse(is.na(max_3_mo_avg) & max_result < Chla_Criteria, "Cat3",
                           ifelse(is.na(max_3_mo_avg) & max_result > Chla_Criteria, "Cat3b", 
                                  ifelse(max_3_mo_avg > Chla_Criteria, "Cat5",
                                         ifelse(max_3_mo_avg <= Chla_Criteria, "Cat2", "ERROR" ))))) %>%
  select(AU_ID, OWRD_Basin,MonLocType, Char_Name, Chla_Criteria, IR_category, 
         max_result,max_mo_avg, max_3_mo_avg )

return(chl_categories)

}