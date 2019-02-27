require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)

#disable scientific notation 
options(scipen = 999)



Coastal_Contact_rec <- function(df){
  
  print("Begin coastal contact rec analysis")
  
  #create lists to get data out of for loops
  geomeanlist = list()
  
  
  
  # Water Contact Recreation - Coastal -----------------------------------
  
  # Get filter down to data needed only for coastal contact rec data
  # Bacteria code #2 and Entero 
  Coastal <- df %>%
    filter(BacteriaCode %in%  c(1, 3),
           Char_Name == "Enterococcus") %>%
    #add blank columns to be filled in during analysis phase
    mutate(geomean = "",
           count_period = "",
           n_above_crit = "",
           perc_above_crit_10 = "",
           n_samples_greater_perc_crit = "",
           less_5 = "",
           Max_value = "",
           SS_Crit = NA,
           Geomean_Crit = 35,
           Perc_Crit = 130)
  
  
  if(length(unique(Coastal$AU_ID)) == 0) {
    stop("No Enterococcus Data")
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
  
  pb <- txtProgressBar(0, length(unique(Coastal$AU_ID)), style = 3)
  
  for(i in 1:length(unique(Coastal$AU_ID))){
    setTxtProgressBar(pb, i)
    station <- unique(Coastal$AU_ID)[i]
    
    # Filter table down to single station
    Coastal_singlestation <- Coastal %>%
      filter(AU_ID == station) %>%
      mutate(geomean_start_date = as.Date(SampleStartDate)-90)
    
    for(j in 1:nrow(Coastal_singlestation)){
      
      #start of 90 day window
      geomean_date <- Coastal_singlestation$geomean_start_date[j]
      # end of 90 day window
      enddate <- Coastal_singlestation$SampleStartDate[j]
      
      #create table for only samples in that window
      geomean_period <- Coastal_singlestation %>%
        filter(SampleStartDate <= enddate & SampleStartDate >= geomean_date )
      
      count_period = nrow(geomean_period)
      
      #get geomeans if number of samples in that window is 5 or greater
      Coastal_singlestation[j,"geomean"] <- ifelse(nrow(geomean_period) >= 5, geo_mean(geomean_period$Result_cen), NA)
      #get count of 90 day period
      Coastal_singlestation[j,"count_period"] <- count_period
      # get number that are above 130 criterion 
      Coastal_singlestation[j,"n_above_crit"] <- sum(geomean_period$Result_cen > 130) 
      # get percent that are above criteria if more than 10 samples in 90 day period
      Coastal_singlestation[j,"perc_above_crit_10"] <- ifelse(count_period >= 10, sum(geomean_period$Result_cen > 130) /count_period, NA)
      # get lowest value in 90 day window if 5-9 samples in 90 day window
      Coastal_singlestation[j,"n_samples_greater_perc_crit"]  <- ifelse(count_period < 10 & count_period >= 5, sum(geomean_period$Result_cen > 130), NA )
      # flag if less than 5 in 90 day window
      Coastal_singlestation[j,"less_5"] <- ifelse(nrow(geomean_period) < 5, 1, 0)
      #Max Value
      Coastal_singlestation[j,"Max_value"] <- max(geomean_period$Result_cen)
      
      
    }
    
    geomeanlist[[i]] <- Coastal_singlestation
    
  }
  
  close(pb)
  
  #Bind list to dataframe and ensure numbers are numeric
  Coastal_analysis <- bind_rows(geomeanlist) %>%
    mutate(geomean = as.numeric(geomean),
           count_period = as.numeric(count_period),
           n_above_crit = as.numeric(n_above_crit),
           perc_above_crit_10 = as.numeric(perc_above_crit_10),
           n_samples_greater_perc_crit = as.numeric(n_samples_greater_perc_crit ),
           less_5 = as.numeric(less_5))
  
  
  # Data review -------------------------------------------------------------
  
  
  IR_export(Coastal_analysis, "Parameters/Bacteria/Data Review", "Bacteria_Coast_Contact", "data" )
  
  # do the conparisons listed in methodology
  Coastal_AU_summary <-  Coastal_analysis %>%
    group_by(AU_ID) %>%
    # list out the maxium geometric mean per AU
    summarise(OWRD_Basin = first(OWRD_Basin), 
              Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
              # maximum percentage of results within geomean groups with more 10 samples that are above criteria 
              max.perc_above_crit_10 =  ifelse(!all(is.na(perc_above_crit_10)),max(perc_above_crit_10, na.rm = TRUE),NA),
              # maximum percentage of results within geomean groups with more 5 samples that are above criteria 
              max.n_samples_greater_perc_crit = ifelse(all(is.na(n_samples_greater_perc_crit)), NA, max(n_samples_greater_perc_crit, na.rm= TRUE)),
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
                                  (!is.na(max.n_samples_greater_perc_crit) & max.n_samples_greater_perc_crit >= 2), "Cat5", 
                                ifelse(perc.insuff == 1 & max.value < Perc_Crit, "Cat3", 
                                       ifelse(perc.insuff == 1 & max.value > Perc_Crit, "Cat3B", 
                                              "Cat2")))) 
  
  
  
  
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
  
  print("Finish coastal contact rec analysis")
  return(Coastal_AU_summary)
}
