

#Build constants table to use to join
constants <- data.frame("Char_Name" = c('Cadmium', 'Chromium', 'Lead', 'Nickel', 'Silver', 'Zinc'),
               "ma" = c(NA, 0.8190, 1.273, 0.8460, 1.72, 0.8473),
               "ba" = c(NA, 3.7256, -1.460, 2.255, -6.59, 0.884),
               "mc" = c(0.7409, .08190, 1.273, 0.8460, NA, 0.8473),
               'bc' = c(-4.719, 0.6848, -4.705, 0.0584, NA, 0.884), stringsAsFactors = FALSE)



Hardness_analysis <- metals_hardness %>%
  left_join(constants, by = "Char_Name") %>%
  mutate(CF = ifelse(Char_Name == 'Cadmium', 1.101672-  (log(crit_hardness) * 0.041838), 
                     ifelse(Char_Name == 'Chromium', 0.860, 
                            ifelse(Char_Name == 'Lead', 1.46203 - (log(crit_hardness) * 0.145712), 
                                   ifelse(Char_Name == 'Nickel', 0.997, 
                                          ifelse(Char_Name == 'Silver', 0.85, 
                                                ifelse(Char_Name == 'Zinc', 0.986, "ERROR" ) )))))) %>%
  mutate(CF = as.numeric(CF)) %>%
  mutate(crit = ifelse(Char_Name == 'Silver' & (exp(ma*(log(crit_hardness)+ba))*CF) > 0.10, 0.10, 
                       ifelse(Char_Name == 'Silver' & (exp(ma*(log(crit_hardness)+ba))*CF) < 0.10, exp(ma*(log(crit_hardness)+ba))*CF, 
                              exp(mc*(log(crit_hardness)+bc))*CF )))


Results_censored <- Censored_data(Hardness_analysis, crit = `crit` ) %>%
  mutate(excursion = ifelse(IRResultNWQSunit > crit , 1, 0 ))
         
      