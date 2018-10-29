

Shell_Harvest <- function(df) {
  
  print("Begin shellfish harvesting analysis")
  
  shell_harvest <- df %>%
    filter(BacteriaCode == 3,
           ChrName == "Fecal Coliform") %>%
    mutate(perc_exceed = ifelse(Result_cen < Perc_Crit, 1, 0))
  
  if(nrow(shell_harvest) == 0) {
    stop("No available data")
  }
  
  shell_harvest_analysis <- shell_harvest %>%
    group_by(AU_ID) %>%
    summarise(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_exceed = sum(perc_exceed),
              Perc_Crit = first(Perc_Crit),
              SS_Crit = first(SS_Crit)
              )

  
  # Data review -------------------------------------------------------------
  
  
  # Get list of unique basins in dataset. Used for generating data for review
  basins <- unique(shell_harvest_analysis$OWRD_Basin) 
  
  
  # Loop through data, and filter by OWRD basin, write csv file of all data in that basin
  for(i in 1:length(basins)){
    
    Basin <- basins[i]
    
    bacteria_shell_contact_analysis_by_basin <-  shell_harvest_analysis %>%
      filter(OWRD_Basin == Basin)
    
    write.csv(bacteria_shell_contact_analysis_by_basin, paste0("Parameters/Bacteria/Data Review/Shell_Harvest_IR_data_",Basin,".csv"))
    
  }
  
    
  shell_harvest_summary <- shell_harvest_analysis %>%
    mutate(IR_category = ifelse((!is.na(median) & median > SS_Crit) | 
                                  (num_samples >= 10 & num_exceed/num_samples > 0.10) |
                                  (num_samples >= 5 & num_samples <= 9 &  num_exceed >= 1 ), "Cat5", 
                               ifelse(num_samples < 5 & num_exceed == 0, "Cat3", 
                                      ifelse(num_samples < 5 & num_exceed > 0, "Cat3b", 
                                             ifelse((!is.na(median) & median < SS_Crit & num_exceed/num_samples <= 0.10 & num_samples >= 10) |
                                                      (!is.na(median) & median < SS_Crit & num_samples >= 5 & num_samples <= 9 & num_exceed == 0), "Cat2", "ERROR" ))) ))
    
    
    # mutate(Cat5 = ifelse((!is.na(median) & median > SS_Crit) | 
    #                      (num_samples >= 10 & num_exceed/num_samples > 0.10) |
    #                      (num_samples >= 5 & num_samples <= 9 &  num_exceed >= 1 ), 1, 0 ),
    #        Cat3 = ifelse(num_samples < 5 & num_exceed == 0, 1, 0 ),
    #        Cat3b = ifelse(num_samples < 5 & num_exceed > 0, 1, 0 ),
    #        Cat2 = ifelse((!is.na(median) & median < SS_Crit & num_exceed/num_samples <= 0.10 & num_samples >= 10) |
    #                      (!is.na(median) & median < SS_Crit & num_samples >= 5 & num_samples <= 9 & num_exceed == 0), 1, 0)
    #        )
  
  return(shell_harvest_summary)

}