library(tidyverse)
library(lubridate)
library(zoo)




chl_assessment <- function(df){

chla_data <- df %>%
  #filter(!is.na(AU_ID)) %>%
  mutate( month = month(ActStartD),
          yrfromstart = year(ActStartD) - 2008,
          monthfromstart = month + 12*yrfromstart) 

chla_mo_avg <- chla_data %>%
  arrange(AU_ID,monthfromstart ) %>%
  group_by(AU_ID, monthfromstart, Chla_Criteria, OWRD_Basin, ChrName) %>%
  summarise(monthaverage = mean(Result4IR))
  




# Figure out where we have 3 consecutive monthly averages -----------------


library(data.table)
chla_data_table <-  data.table(chla_mo_avg) 

# re order
chla_data_table <- setkey(chla_data_table[order(monthfromstart)], AU_ID)

# compute diffs
chla_data_table[, diffs := c(0, diff(monthfromstart)), by=AU_ID]


## We will use cumsum.  Anything greater than 1, should be reset to 0
chla_data_table[diffs > 1, diffs := 9999]


# Reset as dataframe
chla_data_table <- as.data.frame(chla_data_table)




chla_avgs <- chla_data_table %>%
  mutate(consecutive3 = ifelse((lag(diffs , 2) != 9999) & (lag(diffs , 1) != 9999)  & diffs!= 9999, 1, 0 ) ) %>%
  group_by(AU_ID) %>%
  mutate(avg.3.mo = ifelse(consecutive3 == 1, rollmean(monthaverage,
                                                       3,
                                                       align = 'right',
                                                       fill= NA,
                                                       na.rm = T), NA ) )

chla_data_analysis <-  chla_data %>%
  left_join(chla_avgs, by = c("AU_ID", "monthfromstart", "Chla_Criteria", "OWRD_Basin", "ChrName")) %>%
  select(-diffs, -consecutive3)



basins <- unique(chla_data_analysis$OWRD_Basin) 

# Loop through data, and filter by OWRD basin, write csv file of all data in that basin
for(i in 1:length(basins)){
  
  Basin <- basins[i]
  
  chla_analysis_by_basin <-  chla_data_analysis %>%
    filter(OWRD_Basin == Basin)
  
  write.csv(chla_analysis_by_basin, paste0("Parameters/chl_a/Data_Review/Chla_IR_data_",Basin,".csv"))
  
}


chl_categories <- chla_data_analysis %>%
  group_by(AU_ID, MonLocType, OWRD_Basin, ChrName, Chla_Criteria) %>%
  summarise(max_result = max(Result_cen),
            max_mo_avg = max(monthaverage),
            max_3_mo_avg = max(avg.3.mo)) %>%
  mutate(IR_category = ifelse(is.na(max_3_mo_avg) & max_result < Chla_Criteria, "Cat3",
                           ifelse(is.na(max_3_mo_avg) & max_result > Chla_Criteria, "Cat3b", 
                                  ifelse(max_3_mo_avg > Chla_Criteria, "Cat5",
                                         ifelse(max_3_mo_avg <= Chla_Criteria, "Cat2", "ERROR" ))))) %>%
  select(AU_ID, OWRD_Basin,MonLocType, ChrName, Chla_Criteria, IR_category, 
         max_result,max_mo_avg, max_3_mo_avg )

return(chl_categories)

}