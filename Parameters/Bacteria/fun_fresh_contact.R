require(rgdal)
require(RODBC)
require(tidyverse)
require(IRlibrary)



Fresh_Contact_rec <- function(df){
  print("Begin fresh contact rec analysis")
  
  #create lists to get data out of for loops
  geomeanlist = list()
  
  
  fresh_contact <- df %>%
    filter(BacteriaCode == 2,
           Char_Name == "Escherichia coli") %>%
    #add blank columns to be filled in during analysis phase
    mutate(geomean = "",
           count_period = "",
           n_above_crit = "",
           perc_above_crit_10 = "",
           perc_above_crit_5 = "",
           less_5 = "",
           Max_value = "")
  
  
  if(length(unique(fresh_contact$AU_ID)) == 0) {
    stop("No E coli Data")
  } 
  
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
  
  pb <- txtProgressBar(0, length(unique(fresh_contact$AU_ID)), style = 3)
  
  for(i in 1:length(unique(fresh_contact$AU_ID))){
    setTxtProgressBar(pb, i)
    station <- unique(fresh_contact$AU_ID)[i]
    
    # Filter table down to single station
    fresh_singlestation <- fresh_contact %>%
      filter(AU_ID == station) %>%
      mutate(geomean_start_date = as.Date(SampleStartDate)-90)
    
    for(j in 1:nrow(fresh_singlestation)){
      
      #start of 90 day window
      geomean_date <- fresh_singlestation$geomean_start_date[j]
      # end of 90 day window
      enddate <- fresh_singlestation$SampleStartDate[j]
      
      #create table for only samples in that window
      geomean_period <- fresh_singlestation %>%
        filter(SampleStartDate <= enddate & SampleStartDate >= geomean_date )
      
      count_period = nrow(geomean_period)
      
      #get geomeans if number of samples in that window is 5 or greater
      fresh_singlestation[j,"geomean"] <- ifelse(nrow(geomean_period) >= 5, geo_mean(geomean_period$Result_cen), NA)
      #get count of 90 day period
      fresh_singlestation[j,"count_period"] <- count_period
      # get number that are above 130 criterion 
      fresh_singlestation[j,"n_above_crit"] <- sum(geomean_period$Result_cen > 406) 
      # get percent that are above criteria if more than 10 samples in 90 day period
      fresh_singlestation[j,"perc_above_crit_10"] <- ifelse(count_period >= 10, sum(geomean_period$Result_cen > 406) /count_period, NA)
      # get lowest value in 90 day window if 5-9 samples in 90 day window
      fresh_singlestation[j,"perc_above_crit_5"]  <- ifelse(count_period < 10 & count_period >= 5, max(geomean_period$Result_cen), NA )
      # flag if less than 5 in 90 day window
      fresh_singlestation[j,"less_5"] <- ifelse(nrow(geomean_period) < 5, 1, 0)
      #Max Value
      fresh_singlestation[j,"Max_value"] <- max(geomean_period$Result_cen)
      
      
    }
    
    geomeanlist[[i]] <- fresh_singlestation
    
  }
  
  close(pb)
  
  #Bind list to dataframe and ensure numbers are numeric
  fresh_analysis <- bind_rows(geomeanlist) %>%
    mutate(geomean = as.numeric(geomean),
           count_period = as.numeric(count_period),
           n_above_crit = as.numeric(n_above_crit),
           perc_above_crit_10 = as.numeric(perc_above_crit_10),
           perc_above_crit_5 = as.numeric(perc_above_crit_5 ),
           less_5 = as.numeric(less_5))
  
  
  # Data review -------------------------------------------------------------
  
  
  # Get list of unique basins in dataset. Used for generating data for review
  basins <- unique(fresh_analysis$OWRD_Basin) 
  
  
  # Loop through data, and filter by OWRD basin, write csv file of all data in that basin
  for(i in 1:length(basins)){
    
    Basin <- basins[i]
    
    bacteria_coast_analysis_by_basin <-  fresh_analysis %>%
      filter(OWRD_Basin == Basin)
    
    write.csv(bacteria_coast_analysis_by_basin, paste0("Parameters/Bacteria/Data Review/Coast_Contact_IR_data_",Basin,".csv"))
    
  }  
  
  
  # do the conparisons listed in methodology
  fresh_AU_summary <-  fresh_analysis %>%
    group_by(AU_ID) %>%
    # list out the maxium geometric mean per AU
    summarise(Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
              # maximum percentage of results within geomean groups with more 10 samples that are above criteria 
              max.perc_above_crit_10 =  ifelse(!all(is.na(perc_above_crit_10)),max(perc_above_crit_10, na.rm = TRUE),NA),
              # maximum percentage of results within geomean groups with more 5 samples that are above criteria 
              max.perc_above_crit_5 = ifelse(!all(is.na(perc_above_crit_5)),max(perc_above_crit_5, na.rm= TRUE),NA),
              # percent of samples that do not have 5 or more samples in each 90 day period. 
              #Used to determine if a 90 day geomean is even possible for cat 3 or cat 3b
              perc.insuff = sum(less_5)/n(),
              #Maximum value of AU group. Used for 3 or 3b
              max.value  = max(Result_cen),
              SS_Crit = max(SS_Crit),
              Geomean_Crit = max(Geomean_Crit),
              Perc_Crit = max(Perc_Crit)) %>%
    # Cat 5 is max geomean is > 35 or no geomean group has more than 10% samples above perc crit 
    mutate(IR_category = ifelse((!is.na(Max_Geomean) & Max_Geomean > Geomean_Crit) | 
                                  (!is.na(max.perc_above_crit_10) & max.perc_above_crit_10 > 0.10) | 
                                  (!is.na(max.perc_above_crit_5) & max.perc_above_crit_5 > Perc_Crit), "Cat5", "other")) %>%
    mutate(IR_category =ifelse(IR_category == "Cat5", "Cat5",
                               ifelse(perc.insuff == 1 & max.value < Perc_Crit, "Cat3", 
                                       ifelse(IR_category != "Cat5" & perc.insuff == 1 & max.value > Perc_Crit, "Cat3B", 
                                              ifelse((
                                                !is.na(Max_Geomean) &
                                                  Max_Geomean <= Geomean_Crit &
                                                  perc.insuff < 1 & 
                                                  !is.na(max.perc_above_crit_10) & max.perc_above_crit_10 < 0.10) |
                                                  (!is.na(Max_Geomean) &
                                                     Max_Geomean <= Geomean_Crit &
                                                     perc.insuff < 1 &
                                                     !is.na(max.perc_above_crit_5) & max.perc_above_crit_5 < Perc_Crit), "Cat2", "ERROR" )))))
  
  
  
  
  # mutate(Cat5 = ifelse((!is.na(Max_Geomean) & Max_Geomean > Geomean_Crit) | 
  #                        (!is.na(max.perc_above_crit_10) & max.perc_above_crit_10 > 0.10) | 
  #                        (!is.na(max.perc_above_crit_5) & max.perc_above_crit_5 > Perc_Crit), 1, 0),
  #       #Cat 3 if not cat 5 AND not enough sanples to calculte a geomean AND max value in dataset is less than 130
  #        Cat3 = ifelse(Cat5 !=1 & perc.insuff == 1 & max.value < Perc_Crit, 1, 0),
  #       #Cat 3b if not cat 5 AND not enough sanples to calculte a geomean AND max value in dataset is greater than 130
  #        Cat3B = ifelse(Cat5 !=1 & perc.insuff == 1 & max.value > Perc_Crit, 1, 0),
  #       #Cat 2 if able to calculte a geomean and max geomean is less than 35 and  max.perc_above_crit_10 < 0.10) 
  #       #OR  able to calculte a geomean and max geomean is less than 35 and max.perc_above_crit_5 < 130
  #        Cat2 = ifelse((
  #          !is.na(Max_Geomean) &
  #            Max_Geomean <= Geomean_Crit &
  #            perc.insuff < 1 & 
  #            !is.na(max.perc_above_crit_10) & max.perc_above_crit_10 < 0.10) |
  #            (!is.na(Max_Geomean) &
  #               Max_Geomean <= Geomean_Crit &
  #               perc.insuff < 1 &
  #               !is.na(max.perc_above_crit_5) & max.perc_above_crit_5 < Perc_Crit),1,0)
  # )
  
  print("Finish fresh contact rec analysis")
  return(fresh_AU_summary)
}
