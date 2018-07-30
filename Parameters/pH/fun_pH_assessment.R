library(tidyverse)


pH_assessment <- function(df) {
  pH_summary <- df %>%
    mutate(pH_violation = ifelse(Result_cen < pH_Min | Result_cen > pH_Max, 1, 0 )) %>%
    group_by(AU_ID) %>%
    summarise(num_Samples = n(),
              num_violation = sum(pH_violation),
              pH_Min = min(pH_Min),
              pHMax = max(pH_Max),
              pH_code = first(pH_code)) %>%
    mutate(Cat5 = ifelse(num_Samples > 5 & num_violation/num_Samples > 0.10 & num_violation >= 2, 1, 0),
           Cat3 = ifelse((num_Samples < 5 & num_violation < 2) | 
                           ((num_Samples >= 5 & num_Samples <= 9) & num_violation == 1),1,0),
           Cat3B = ifelse(num_Samples < 5 & num_violation >= 2, 1, 0),
           Cat2 = ifelse((num_Samples >= 10 &  num_violation/num_Samples <= 0.10) | 
                           ((num_Samples >= 5 & num_Samples <= 9) & num_violation == 0), 1, 0)
           )
  return(pH_summary)
}

