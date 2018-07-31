

Shell_Harvest <- function(df) {
  
  print("Begin shellfish harvesting analysis")
  
  shell_harvest <- df %>%
    filter(BacteriaCo == 3) %>%
    #       ChrName == "Fecal Coliform") %>%
    mutate(perc_exceed = ifelse(Result_cen < Perc_Crit, 1, 0))
  
  shell_harvest_analysis <- shell_harvest %>%
    group_by(AU_ID) %>%
    summarise(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_exceed = sum(perc_exceed),
              Perc_Crit = first(Perc_Crit),
              SS_Crit = first(SS_Crit)
              )
  
  shell_harvest_summary <- shell_harvest_analysis %>%
    mutate(Cat5 = ifelse((!is.na(median) & median > SS_Crit) | 
                         (num_samples >= 10 & num_exceed/num_samples > 0.10) |
                         (num_samples >= 5 & num_samples <= 9 &  num_exceed >= 1 ), 1, 0 ),
           Cat3 = ifelse(num_samples < 5 & num_exceed == 0, 1, 0 ),
           Cat3b = ifelse(num_samples < 5 & num_exceed > 0, 1, 0 ),
           Cat2 = ifelse((!is.na(median) & median < SS_Crit & num_exceed/num_samples <= 0.10 & num_samples >= 10) |
                         (!is.na(median) & median < SS_Crit & num_samples >= 5 & num_samples <= 9 & num_exceed == 0), 1, 0)
           )
  
  return(shell_harvest_summary)

}