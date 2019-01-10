

TOX_AL_analysis <- function(df){
  
# Pull data out for summing -----------------------------------------------


# DDT ---------------------------------------------------------------------

DDT_data <- df %>%
  filter(Pollu_ID %in% c(48,49,50)) %>%
  mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen)) %>%
  group_by(OrganizationID, MLocID, SampleStartDate, SampleStartTime, Analytical_method, act_depth_height) %>%
  mutate(IR_note = "Sum of DDT and metabolites",
         Summed_values = sum(summed_censored_value),
         summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
  # Keep only the first row. This preserves all the metadata
  filter(row_number() == 1) %>%
  # Change the Char_Name to DDT and the Result_cen column to the summed value
  mutate(Char_Name = "DDT",
         Result_cen = Summed_values) %>%
  # get rid of extra columns that were created
  select(-Summed_values, -summed_censored_value)
  

# Endosulfan data ---------------------------------------------------------


endosulfan_data <- df %>%
  # Filter for endosulfan Pollu_IDs
  filter(Pollu_ID %in% c(77,78,79 )) %>%
  # Set a flag for if the value is total_endosulfan
  mutate(is_total_endosulfan = ifelse(Pollu_ID == 77, 1, 0 ),
         summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen)) %>%
  # Set a group that identifies a single sample
  group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
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



# Chlordane ---------------------------------------------------------------

# Get only chlordate data
# mark if result is  total chlordane  
Chlordane <- df %>%
  filter(Pollu_ID %in% c(27)) %>%
  mutate(is_total = ifelse(chr_uid %in% c(767), 1, 0 ),
         summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen)) %>%
# Set a group that identifies a single sample
  group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
  # Flag if the group has a total endosulfan result 
  mutate(has_total_chlordane = ifelse(max(is_total) == 1, 1, 0)) %>%
  # remove isomers, metabolites, etc if the group has a total
  filter((has_total_chlordane == 1 & is_total == 1) | has_total_chlordane == 0) %>%
  mutate( summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
  mutate(IR_note = ifelse(chr_uid != 767, "Sum of isomers, metabolites, and other constituents", "" ),
         Summed_values = ifelse(is_total == 1, IRResultNWQSunit, sum(summed_censored_value) ) ) %>%
  # Keep only the first row. This preserves all the metadata
  filter(row_number() == 1) %>%
  # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
  mutate(Char_Name = "Chlordane",
         Result_cen = Summed_values) %>%
  select(-Summed_values, is_total, summed_censored_value,has_total_chlordane )
  
# PCB data ----------------------------------------------------------------

PCB_data <- df  %>%
  filter(Pollu_ID == '153') %>%
  #Identufy the aroclors
  mutate(is_aroclor = ifelse(chr_uid %in% c('575','578','580','582','583','586','587'), 1, 0 )) %>% #These are the uid for the Arochlors
  # Group by org, mloc, date, and depth to identify sampling event
  group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
  # Flag of the grouping has an arochlor sample
  mutate(Has_aroclor = ifelse(max(is_aroclor) == 1, 1, 0)) %>%
  # Undo the grouping
  ungroup() %>%
  # keep the type (aroclor or congener) that has the least amount of non-detects by percentage
  # Group by the same as above, but add in the is_arochlor flag
  group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height, is_aroclor) %>%
  # Calculate the percent nondetect of each group
  mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
  #undo the grouping
  ungroup() %>%
  # redo the original single sample grouping 
  group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
  # remove individual congeners if the group has arochlor data & the aroclors have a lower percentage of nondetects
  filter((Has_aroclor == 1 & is_aroclor == 1 & summed_percent_nondetect == min(summed_percent_nondetect)) | Has_aroclor == 0) %>%
  # Recalculate the percent censored values
  mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen )) %>%
  mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
         # Do the summing
         Summed_values = sum(summed_censored_value),
         # Create note on what the summing is based on
         IR_note = ifelse(Has_aroclor ==  1, "PCB - Sum of Aroclors", 
                          ifelse(Has_aroclor ==  0, "PCB - Sum of congeners", "ERROR" )),
         Result_Operator = max(Result_Operator)
  ) %>%
  # Keep only the first row. This preserves all the metadata
  filter(row_number() == 1) %>%
  # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
  mutate(Char_Name = "PCBs",
         Result_cen = Summed_values) %>%
  # get rid of extra columns that were created
  select(-Summed_values,  -Has_aroclor,  -is_aroclor, -summed_censored_value)


# Put data back together --------------------------------------------------


results_analysis <- df %>%
  filter(!Pollu_ID %in% c(77,78,79 )) %>%
  filter(Pollu_ID != 153) %>%
  filter(!Pollu_ID %in% c(48,49,50)) %>%
  filter(Pollu_ID != 27) %>%
  bind_rows(endosulfan_data) %>%
  bind_rows(PCB_data) %>%
  bind_rows(DDT_data) %>%
  bind_rows(Chlordane)


Results_tox_AL_analysis <- results_analysis %>%
   arrange(OrganizationID, MLocID, Char_Name, SampleStartDate,SampleStartTime) %>%
  # Create column for simplfied version of sample fraction
  # THese distinctions came from Sara Krepps
  mutate(Simplified_sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual", 
                                                                    "None", "volatile", "Semivolatile")  |
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
         excursion = ifelse(Char_Name %in% c("Alkalinity, total", "Alkalinity, bicarbonate")  & evaluation_result < evaluation_crit, 1, 
                            ifelse(!(Char_Name %in% c("Alkalinity, total", "Alkalinity, bicarbonate")) & evaluation_result > evaluation_crit, 1, 0 )))
   

IR_export(Results_tox_AL_analysis, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Others", "Data")

Results_tox_AL_categories <- Results_tox_AL_analysis %>%
  group_by(AU_ID, Char_Name) %>%
  #Summarise data
  summarise(OWRD_Basin = first(OWRD_Basin),
            criteria_fraction = first(Fraction),
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
                                                          ifelse(num_excursions_all <= critical_excursions, "Cat 2", "ERROR" )))) ) ),
         percent_3d = ifelse(is.na(summed_percent_nondetect), percent_3d, NA ))

IR_export(Results_tox_AL_categories, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Others", "Categories")
                                    

# test_pivot <- Results_tox_AL_categories %>%
#   select(AU_ID, Char_Name, IR_category) %>%
#   spread(key =  Char_Name, value =   IR_category)

return(Results_tox_AL_categories)
                                    
}                             



#TO do
# Chlordane
    # Need to update list from Lori