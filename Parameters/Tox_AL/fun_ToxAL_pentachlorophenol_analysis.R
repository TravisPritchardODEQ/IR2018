library(IRlibrary)
library(lubridate)


# Assign violations
penta_data_analysis <- Penta_data %>%
  mutate(violation = ifelse(Result_cen > CMC_crit, 1, 0 ))

#Write data review tables
# Get list of unique basins in dataset. Used for generating data for review
basins <- unique(penta_data_analysis$OWRD_Basin) 


# Loop through data, and filter by OWRD basin, write csv file of all data in that basin
for(i in 1:length(basins)){
  
  Basin <- basins[i]
  print(paste("Writing table", i, "of",length(basins), "-", Basin ))
  
  analysis_by_basin <-  penta_data_analysis %>%
    filter(OWRD_Basin == Basin)
  
  write.csv(analysis_by_basin, paste0("Parameters/Tox_AL/Data_Review/Pentachlorophenol_IR_data_",Basin,".csv"))
  
}


write.csv(analysis_by_basin, "Parameters/Tox_AL/Data_Review/Pentachlorophenol_IR_data_ALL_DATA.csv")

#Summarize data and assign critical excursions and IR category
penta_data_summary <- penta_data_analysis %>%
  group_by(AU_ID) %>%
  summarise(num_samples = n(),
            num_Violations = sum(violation),
            percent_3D = round(sum(Result_Operator == "<" & MRLValue > CMC_crit )/num_samples * 100)) %>%
  mutate(critical_excursions = excursions_tox(num_samples),
         Category = ifelse(percent_3D == 100, "Cat 3D", 
                           ifelse(num_Violations > critical_excursions, "Cat 5", "Cat 2" )))

