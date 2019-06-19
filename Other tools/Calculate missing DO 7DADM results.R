library(tidyverse)

load("Parameters/DO/Validated_results.Rdata")


filtered_data <- Validated_results %>%
  filter(AU_ID %in% c('OR_SR_1709000702_02_104007',
                      'OR_WS_170900070204_02_104412',
                      'OR_WS_170900070301_02_104413',
                      'OR_WS_170900070303_02_104415')) %>%
  filter(Statistical_Base == 'Mean') %>%
  arrange(MLocID, SampleStartDate)

monloc_do_list <- list()

for(j in 1:length(unique(filtered_data$MLocID))){
  
  print(paste("Station", j, "of", length(unique(filtered_data$MLocID))))
  
  station <- unique(filtered_data$MLocID)[j]
  
  
  #Filter dataset to only look at 1 monitoring location at a time
  daydat_station <- filtered_data %>%
    filter(MLocID == station) %>%
    mutate(startdate7 = as.Date(SampleStartDate) - 6,
           startdate30 = as.Date(SampleStartDate) -30)
  
  print("Begin 7 day moving averages")
  pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
  for(k in 1:nrow(daydat_station)){
    
    start7 <- daydat_station$startdate7[k]
    end7 <- daydat_station$SampleStartDate[k] 
    
    station_7day <- daydat_station %>%
      filter(SampleStartDate <= end7 & SampleStartDate >= start7) 
    
    
    ma.mean7 <- ifelse(length(unique(station_7day$SampleStartDate)) >= 6, mean(station_7day$IRResultNWQSunit), NA )
  
    
    daydat_station[k,"ma.mean7"] <- ifelse(k >=7, ma.mean7, NA)
   
    
    
    setTxtProgressBar(pb, k)
  }
  
  close(pb)
  monloc_do_list[[j]] <- daydat_station
  
}

  
  



sum_stats <- bind_rows(monloc_do_list)  


data_to_add_missing_7DADM <- sum_stats %>%
  filter(!is.na(ma.mean7)) %>%
  mutate(IRResultNWQSunit = ma.mean7,
         Statistical_Base = '7DADMean',
         Time_Basis = '7 Day') %>%
  select(-startdate7,
         -startdate30,
         -ma.mean7) %>%
  mutate(analysis_comment = "This metric was missing from initial data pull. Calculated during assessment process")

save(data_to_add_missing_7DADM, file = "Parameters/DO/missing_7DADM_results.RData")
