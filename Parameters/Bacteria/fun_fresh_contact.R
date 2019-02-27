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
           count_period = "")
  
  
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
    #print(paste("i = ", i))
    
    for(j in 1:nrow(fresh_singlestation)){
      
     # print(paste("j = ", j))
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
     
    
      
    }
    
    geomeanlist[[i]] <- fresh_singlestation
    
  }
  
  close(pb)
  
  #Bind list to dataframe and ensure numbers are numeric
  fresh_analysis <- bind_rows(geomeanlist) %>%
    mutate(geomean = as.numeric(geomean),
           count_period = as.numeric(count_period)) %>%
    select(-Perc_Crit)
  
  
  # Data review -------------------------------------------------------------
  
  IR_export(fresh_analysis, "Parameters/Bacteria/Data Review", "Bacteria_Fresh_Contact", "data" )
 
  
  # do the conparisons listed in methodology
  fresh_AU_summary <-  fresh_analysis %>%
    group_by(AU_ID) %>%
    # list out the maxium geometric mean per AU
    summarise(OWRD_Basin = first(OWRD_Basin), 
              Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
              max.value  = max(Result_cen),
              num_Samples = as.numeric(n()),
              num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
              critical_excursions = excursions_conv(num_Samples),
              SS_Crit = max(SS_Crit),
              Geomean_Crit = max(Geomean_Crit)) %>%
    mutate(IR_category = ifelse(!is.na(Max_Geomean) &
                                  (Max_Geomean > Geomean_Crit |
                                  num_ss_excursions > critical_excursions), "Cat5", 
                                ifelse(is.na(Max_Geomean) & max.value < SS_Crit, "Cat3", 
                                       ifelse(is.na(Max_Geomean) & max.value > SS_Crit, "Cat3B", 
                                              "Cat2"))))
  

  
  print("Finish fresh contact rec analysis")
  return(fresh_AU_summary)
}
