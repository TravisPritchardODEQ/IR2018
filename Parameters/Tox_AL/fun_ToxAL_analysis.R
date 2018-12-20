

endosulfan_data <- 


Results_tox_AL_analysis <- Results_censored %>%
  arrange(OrganizationID, MLocID, Char_Name, SampleStartDate,SampleStartTime) %>%
  # Create column for simplfied version of sample fraction
  # THese distinctions came from Sara Krepps
  mutate(Simplified_sample_fraction = ifelse(Sample_Fraction == "Total"  |
                                               Sample_Fraction == "Extractable"  |
                                               Sample_Fraction == "Total Recoverable"  |
                                               Sample_Fraction == "Total Residual"    |
                                               Sample_Fraction == "None"  |
                                               is.na(Sample_Fraction), 'Total', 
                                             ifelse(Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
  group_by(OrganizationID, MLocID, Char_Name, SampleStartDate,SampleStartTime, Analytical_method, act_depth_height) %>%
  # If group has Total fractionin it, mark with a 1. If ony dissolved, mark with 0
  mutate(Has_Crit_Fraction = ifelse(Fraction == "Total" & max(Simplified_sample_fraction) == "Total", 1, 
                                    ifelse(Fraction == "Dissolved" & max(Simplified_sample_fraction) != "Total", 1, 0 ))) %>%
  # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not march
  ungroup() %>%
  filter((Has_Crit_Fraction == 1 & Simplified_sample_fraction == Fraction) | Has_Crit_Fraction == 0) %>%
  # evaluate against the "most stringent of the acute or chronic criterion" - methodology
  # set evaluation criteria to be the lowest of the acute or chronic, depending on water type code
  mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(Acute_FW, Chronic_FW, na.rm = TRUE), pmin(Acute_SW, Chronic_SW, na.rm = TRUE) )) %>%
  # Remove results with null evaluation_criteria (indicating a mimatch between water type and criteria (ex freshwater phosporus samples ))
  filter(!is.na(evaluation_crit)) %>%
  mutate(evaluation_result = ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode == 2, Result_cen*0.8, 
                                    ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode != 2, Result_cen*0.59, Result_cen )),
         excursion = ifelse((Char_Name == "Alkalinity, total" | Char_Name == "Alkalinity, bicarbonate")  & evaluation_result < evaluation_crit, 1, 
                            ifelse(Char_Name != "Alkalinity, total" & evaluation_result > evaluation_crit, 1, 0 ))
 )  
#### Write table here


Results_tox_AL_categories <- Results_tox_AL_analysis %>%
  group_by(AU_ID, Char_Name) %>%
  summarise(criteria_fraction = first(Fraction),
            num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > evaluation_crit )/num_samples * 100),
            num_fraction_types = n_distinct(Simplified_sample_fraction),
            num_samples_total_fraction = sum(Simplified_sample_fraction == "Total"),
            num_Samples_dissolved_fraction = sum(Simplified_sample_fraction == "Dissolved"),
            num_excursions_all = sum(excursion),
            num_excursions_total_fraction = sum(excursion[Simplified_sample_fraction == "Total"]),
            num_excursions_dissolved_fraction = sum(excursion[Simplified_sample_fraction == "Dissolved"]),
            num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction,
                                                     ifelse(Char_Name == "Arsenic", num_samples, 
                                                            num_Samples_dissolved_fraction + (num_samples_total_fraction -num_excursions_total_fraction ) )), 
            critical_excursions = excursions_tox(num_samples_crit_excursion_calc)) %>%
  mutate(IR_category = ifelse(num_samples_crit_excursion_calc == 1 & num_excursions_all == 1, "Cat 3B",
                              ifelse(num_samples_crit_excursion_calc == 1 & num_excursions_all == 0, "Cat 3",
                                     ifelse((Char_Name == "Alkalinity, total" | Char_Name == "Alkalinity, bicarbonate") & num_excursions_all > 0, "Cat 3B", 
                                            ifelse(percent_3d == 100, "Cat 3D",  
                                                   ifelse(num_excursions_all > critical_excursions, "Cat 5", 
                                                          ifelse(num_excursions_all <= critical_excursions, "Cat 2", "ERROR" )))) ) ))
                                    
                                    
                              



#TO do
# Endosulfan - to sum or not to sum?
       # IF sum, seperate out into another table
# Chlordane