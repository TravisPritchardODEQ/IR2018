# This script was used to approximate how many listings we will have using current fresh water listings
# data is pulled stroght from awqms
# No QC checking, so buyer beware
# This script is not inteneded to be used more than the once. 


library(AWQMSdata)
require(tidyverse)
require(IRlibrary)




ecoli_data <- AWQMS_Data(startdate = '2007-01-01', enddate = '2018-07-01', char = c('Escherichia coli') )


print("Begin fresh contact rec analysis")

#create lists to get data out of for loops
geomeanlist = list()


fresh_contact <- ecoli_data %>%
  mutate(geomean = "",
         count_period = "",
         less_5 = "",
         SS_Crit = 406) %>%
  filter(!is.na(Result_Numeric))

# Geometric mean calculations --------------------------------------------


# Process the geometirc means
# These for loops first filter data down to individual monitoring stations
# and sets a variable for each sampling date that indicates the start of a 90 day geomean window.
# The second for loop loops through each activity date and creates a table of all activity dates in that
# 90 day window and calculates the geomettric mean. It then assigns the geomeans into the single location table
# created in the first loop, if there are more than 5 sampling dates in that window. 
# The end of the first loop puts the single location table into a list which is used to bring
# the data out of the for loop by binding it together after the loop into table "Coastal_singlestation"

print("Begin analysis")

pb <- txtProgressBar(0, length(unique(fresh_contact$MLocID)), style = 3)

for(i in 1:length(unique(fresh_contact$MLocID))){
  setTxtProgressBar(pb, i)
  station <- unique(fresh_contact$MLocID)[i]
  
  singlestation <- fresh_contact %>%
    filter(MLocID == station) %>%
    mutate(geomean_start_date = as.Date(SampleStartDate)-90)
  
  for(j in 1:nrow(singlestation)){
    
    #start of 90 day window
    geomean_date <- singlestation$geomean_start_date[j]
    # end of 90 day window
    enddate <- singlestation$SampleStartDate[j]
    
    #create table for only samples in that window
    geomean_period <- singlestation %>%
      filter(SampleStartDate <= enddate & SampleStartDate >= geomean_date )
    
    #number of samples in geomean period
    count_period = nrow(geomean_period)
    
    #get geomeans if number of samples in that window is 5 or greater
    singlestation[j,"geomean"] <- ifelse(nrow(geomean_period) >= 5, geo_mean(geomean_period$Result_Numeric), NA)
    #get count of 90 day period
    singlestation[j,"count_period"] <- count_period
    # flag if less than 5 in 90 day window
    singlestation[j,"less_5"] <- ifelse(nrow(geomean_period) < 5, 1, 0)
    
  }
  geomeanlist[[i]] <- singlestation
  
}

close(pb)

fresh_analysis <- bind_rows(geomeanlist) %>%
  mutate(geomean = as.numeric(geomean),
         count_period = as.numeric(count_period),
         less_5 = as.numeric(less_5)) %>%
  mutate(Geomean_Crit = 126)



# Data review -------------------------------------------------------------


# Get list of unique basins in dataset. Used for generating data for review
#basins <- unique(fresh_analysis$OWRD_Basin) 


# # Loop through data, and filter by OWRD basin, write csv file of all data in that basin
# for(i in 1:length(basins)){
#   
#   Basin <- basins[i]
#   
#   bacteria_fresh_contact_analysis_by_basin <-  fresh_analysis %>%
#     filter(OWRD_Basin == Basin)
#   
#   write.csv(bacteria_fresh_contact_analysis_by_basin, paste0("Parameters/Bacteria/Data Review/Fresh_Contact_IR_data_",Basin,".csv"))
#   
# }



# Categorization ----------------------------------------------------------


fresh_contact_summary <- fresh_analysis %>%
  group_by(MLocID) %>%
  # list out the maxium geometric mean per AU
  summarise(Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value = ifelse(!all(is.na(Result_Numeric)),max(Result_Numeric, na.rm = TRUE),NA),
            num.samples = n(),
            num.above.std = sum(Result_Numeric > SS_Crit),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit)) %>%
  mutate(IR_category = if_else(is.na(Max_Geomean) & num.above.std == 0, "Cat3", 
                               if_else(is.na(Max_Geomean) & num.above.std == 1, "Cat3B", 
                                       if_else((!is.na(Max_Geomean) &  Max_Geomean > Geomean_Crit) |
                                                 (num.samples >=5 & num.above.std >= 1) |
                                                 (num.above.std >= 2 ), "Cat5", 
                                               if_else(!is.na(Max_Geomean) & Max_Geomean < 126 & num.above.std == 0, "Cat2", "ERROR" ))))
  )

IR_summary <- fresh_contact_summary %>%
  group_by(IR_category) %>%
  summarise(count = n())

write.table(IR_summary, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)

write.csv(fresh_contact_summary, "E:/Data/very_rough_not_official_ecoli_analysis.csv", row.names = FALSE )
print("Finish freshwater contact rec analysis") 