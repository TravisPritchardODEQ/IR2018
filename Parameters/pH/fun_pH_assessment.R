library(tidyverse)
library(IRlibrary)


pH_assessment <- function(df) {
  pH_summary <- df %>%
    mutate(pH_violation = ifelse(Result_cen < pH_Min | Result_cen > pH_Max, 1, 0 ),
           pH_violation_high = ifelse(Result_cen > pH_Max, 1, 0 ),
           pH_violation_low = ifelse(Result_cen < pH_Min, 1, 0 ),
           ) 
  


# Data review exports -----------------------------------------------------

IR_export(pH_summary, "Parameters/pH/Data_Review", "pH", "data" )



# Categorization ----------------------------------------------------------

  
  pH_categories <- pH_summary %>%
    group_by(AU_ID) %>%
    summarise(OWRD_Basin = first(OWRD_Basin), 
              num_Samples = n(),
              num_violation = sum(pH_violation),
              num_violation_high = sum(pH_violation_high),
              num_violation_low = sum(pH_violation_low),
              pH_low_crit = min(pH_Min),
              pH_high_crit = max(pH_Max),
              pH_code = first(pH_code)) %>%
    mutate(critical_excursions = excursions_conv(num_Samples),
           IR_category =  case_when(num_Samples < 5 & num_violation >= 2 ~ 'Cat3B',
                                    num_Samples < 5 & num_violation < 2 ~ 'Cat3',
                                    num_Samples >= 5 & num_violation >= critical_excursions ~ 'Cat5',
                                    num_Samples >= 5 & num_violation < critical_excursions ~ 'Cat2',
                                    TRUE ~ 'ERROR')) 
           
           
         
           
  return(pH_categories)
}




