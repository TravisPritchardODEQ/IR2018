

#Still need to fgure out how to sum censored data:

endosulfan_data <- Results_censored %>%
  # Filter for endosulfan Pollu_IDs
  filter(Pollu_ID %in% c(77,78,79 )) %>%
  # Set a flag for if the value is total_endosulfan
  mutate(is_total_endosulfan = ifelse(Pollu_ID == 77, 1, 0 ),
         summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen)) %>%
  # Set a group that identifies a single sample
  group_by(OrganizationID, MLocID, SampleStartDate,SampleStartTime, Analytical_method, act_depth_height) %>%
  # Flag if the group has a total endosulfan result 
  mutate(Has_total_endosulfan = ifelse(max(is_total_endosulfan) == 1, 1, 0)) %>%
  # undo the grouping so the filter works properly
  #I'm not sure this is needed or not, just to be safe
  ungroup() %>%
  # remove alpha and beta componants if the group has a total
  filter((Has_total_endosulfan == 1 & is_total_endosulfan == 1) | Has_total_endosulfan == 0) %>%
  # regroup
  group_by(OrganizationID, MLocID, SampleStartDate,SampleStartTime, Analytical_method, act_depth_height) %>%
  # Count the number of endosulfan types in the group
  # this is needed to be sure that when we are summing the inidividual componants
  # we have both of them
  mutate(num_types = n_distinct(Char_Name),
         summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
  #Filter out the results where the group has only alpha or beta, but not both
  filter(Pollu_ID == 77 | num_types == 2 ) %>%
  # Create a comment field so we know where we did the summing
  # Also add the values in the group to get the summed value
  # All values in the group willget this value
  mutate(IR_note = ifelse(Pollu_ID %in% c(78,79), "Sum of alpha and beta endosulfan", "" ),
         Summed_values = sum(summed_censored_value)) %>%
  # Keep only the first row. This preserves all the metadata
  filter(row_number() == 1) %>%
  # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
  mutate(Char_Name = "Endosulfan",
         Result_cen = Summed_values) %>%
  # get rid of extra columns that were created
  select(-Summed_values,  -num_types,  -Has_total_endosulfan, -is_total_endosulfan, -summed_censored_value)


results_analysis <- Results_censored %>%
  filter(!Pollu_ID %in% c(77,78,79 )) %>%
  bind_rows(endosulfan_data)
  


Results_tox_AL_analysis <- results_analysis %>%
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
  # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
  ungroup() %>%
  filter((Has_Crit_Fraction == 1 & Simplified_sample_fraction == Fraction) | Has_Crit_Fraction == 0) %>%
  # evaluate against the "most stringent of the acute or chronic criterion" - methodology
  # set evaluation criteria to be the lowest of the acute or chronic, depending on water type code
  mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(Acute_FW, Chronic_FW, na.rm = TRUE), pmin(Acute_SW, Chronic_SW, na.rm = TRUE) )) %>%
  # Remove results with null evaluation_criteria (indicating a mismatch between water type and criteria (ex freshwater phosporus samples ))
  filter(!is.na(evaluation_crit)) %>%
  # For arsenic, there is a conversion factor to convert total recoverable arsenic to inorganic. This does that conversion
  mutate(evaluation_result = ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode == 2, Result_cen*0.8, 
                                    ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode != 2, Result_cen*0.59, Result_cen )),
         #`Label as excursion if the evaluation criteria is above (or below for alkalinity) the criteria
         excursion = ifelse((Char_Name == "Alkalinity, total" | Char_Name == "Alkalinity, bicarbonate")  & evaluation_result < evaluation_crit, 1, 
                            ifelse(Char_Name != "Alkalinity, total" & evaluation_result > evaluation_crit, 1, 0 ))
 )  
#### Write table here


Results_tox_AL_categories <- Results_tox_AL_analysis %>%
  group_by(AU_ID, Char_Name) %>%
  #Summarise data
  summarise(criteria_fraction = first(Fraction),
            num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > evaluation_crit )/num_samples * 100),
            summed_percent_nondetect = sum(summed_percent_nondetect)/n(),
            num_fraction_types = n_distinct(Simplified_sample_fraction),
            num_samples_total_fraction = sum(Simplified_sample_fraction == "Total"),
            num_Samples_dissolved_fraction = sum(Simplified_sample_fraction == "Dissolved"),
            num_excursions_all = sum(excursion),
            num_excursions_total_fraction = sum(excursion[Simplified_sample_fraction == "Total"]),
            num_excursions_dissolved_fraction = sum(excursion[Simplified_sample_fraction == "Dissolved"]),
            num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction,
                                                     ifelse(Char_Name == "Arsenic", num_samples, 
                                                            num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction ) )), 
            critical_excursions = excursions_tox(num_samples_crit_excursion_calc)) %>%
  # Assign categories
  mutate(IR_category = ifelse(percent_3d == 100 | (summed_percent_nondetect == 100 & !is.na(summed_percent_nondetect)), "Cat 3D",
                              ifelse(num_samples_crit_excursion_calc == 1 & num_excursions_all == 1, "Cat 3B",
                                     ifelse((Char_Name == "Alkalinity, total" | Char_Name == "Alkalinity, bicarbonate") & num_excursions_all > 0, "Cat 3B", 
                                            ifelse(num_samples_crit_excursion_calc == 1 & num_excursions_all == 0, "Cat 3", 
                                                   ifelse(num_excursions_all > critical_excursions, "Cat 5", 
                                                          ifelse(num_excursions_all <= critical_excursions, "Cat 2", "ERROR" )))) ) ))
                                    
                                    
                              



#TO do
# Summed constiuents and data censoring. How do we sum up values below detection limit?
# Chlordane
    # Need to update list from Lori