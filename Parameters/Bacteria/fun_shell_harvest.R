

Shell_Harvest <- function(df) {
  
  print("Begin shellfish harvesting analysis")
  
  shell_harvest <- df %>%
    filter(BacteriaCode == 3,
           Char_Name == "Fecal Coliform") %>%
    mutate(perc_exceed = ifelse(Result_cen > Perc_Crit, 1, 0))
  
  if(nrow(shell_harvest) == 0) {
    stop("No available data")
  }
  
  shell_harvest_analysis <- shell_harvest %>%
    group_by(AU_ID, OWRD_Basin) %>%
    summarise(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_exceed = sum(perc_exceed),
              Perc_Crit = first(Perc_Crit),
              SS_Crit = first(SS_Crit)
              )

  
  # Data review -------------------------------------------------------------
  
  
  IR_export(shell_harvest, "Parameters/Bacteria/Data Review", "Bacteria_Shell_harvest", "data" )
  
    
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