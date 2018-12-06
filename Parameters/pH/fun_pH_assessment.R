library(tidyverse)
library(IRlibrary)


pH_assessment <- function(df) {
  pH_summary <- df %>%
    mutate(pH_violation = ifelse(Result_cen < pH_Min | Result_cen > pH_Max, 1, 0 ),
           pH_violation_high = ifelse(Result_cen > pH_Max, 1, 0 ),
           pH_violation_low = ifelse(Result_cen < pH_Min, 1, 0 ),
           ) %>%
    group_by(AU_ID) %>%
    summarise(num_Samples = n(),
              num_violation = sum(pH_violation),
              num_violation_high = sum(pH_violation_high),
              num_violation_low = sum(pH_violation_low),
              pH_low_crit = min(pH_Min),
              pH_high_crit = max(pH_Max),
              pH_code = first(pH_code)) %>%
    mutate(k = excursions_conv(num_Samples),
           IR_category = ifelse(num_violation >= k, 'Cat5', 
                                ifelse((num_Samples < 5 & num_violation < 2) | 
                                         ((num_Samples >= 5 & num_Samples <= 9) & num_violation == 1), 'Cat3', 
                                       ifelse(num_Samples < 5 & num_violation >= 2, 'Cat3B', 
                                              ifelse((num_Samples >= 10 &  num_violation < k) | 
                                                       ((num_Samples >= 5 & num_Samples <= 9) & num_violation == 0), 'Cat2', 
                                                     "ERROR")))))
           
  return(pH_summary)
}




