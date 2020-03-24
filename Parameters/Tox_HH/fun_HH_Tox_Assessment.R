library(lubridate)

fun_Tox_HH_analysis <- function(df){ 

#Need to add componant to check for total chlordane. 
    Chlordane_data <- df %>%
      filter(Pollu_ID == '27') %>%
      # Identofy chlordane
      mutate(is_chlordane = ifelse(chr_uid %in% c('767'), 1, 0 )) %>%
      group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
      # Check to see if group has chlordane
      mutate(has_chlordane = ifelse(max(is_chlordane) == 1, 1, 0 )) %>%
      # Get percentage of non detects
      ungroup() %>%
      filter((has_chlordane == 1 & is_chlordane == 1) | has_chlordane == 0) %>%
      group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
      mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
      mutate(summing_censored_value = ifelse(Result_Operator == "<", 0, Result_cen )) %>%
      mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
             # Do the summing
             Summed_values = sum(summing_censored_value),
             # Create note on what the summing is based on
             IR_note = ifelse(has_chlordane == 1, "", paste("result is sum of ", str_c(Char_Name, collapse  = "; ")) ) )  %>%
  # Keep only the first row. This preserves all the metadata
  filter(row_number() == 1) %>%
  # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
  mutate(Char_Name = "Chlordane",
         Result_cen = Summed_values) %>%
  # get rid of extra columns that were created
  select(-Summed_values,  -summing_censored_value, -has_chlordane,is_chlordane)
             


# PCBs --------------------------------------------------------------------

# Sum PCB Data
  #PCB data is identifed by Pollu_ID
  PCB_data <- df  %>%
  filter(Pollu_ID == '153') %>%
    #Identufy the aroclors
  mutate(is_aroclor = ifelse(chr_uid %in% c('575','578','580','582','583','586','587'), 1, 0 )) %>% #THese are the uid for the Arochlors
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
  mutate(PCB_summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
    #undo the grouping
  ungroup() %>%
    # redo the original single sample grouping 
  group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # remove individual congeners if the group has arochlor data & the aroclors have a lower percentage of nondetects
  filter((Has_aroclor == 1 & is_aroclor == 1 & PCB_summed_percent_nondetect == min(PCB_summed_percent_nondetect)) | Has_aroclor == 0) %>%
    # Recalculate the percent censored values
  mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen )) %>%
  mutate(PCB_summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
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
  


# Put summed data together
results_analysis <- df %>%
  filter(Pollu_ID != 153,
         Pollu_ID != 27) %>%
  bind_rows(#PCB_data, 
    Chlordane_data)



tox_HH_assesment <- results_analysis %>%
  # Set null crit_fraction to "Total"
  mutate(Crit_Fraction = ifelse(is.na(Crit_Fraction), "Total", Crit_Fraction )) %>%
  # Create column for simplfied version of sample fraction
  # THese distinctions came from Sara Krepps
  mutate(Simplified_sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual", 
                                                                    "None", "Volatile", "Semivolatile")  |
                                               is.na(Sample_Fraction), 'Total', 
                                             ifelse(Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
  group_by(OrganizationID, MLocID, Char_Name, SampleStartDate, Analytical_method, act_depth_height) %>%
  # If group has Total fractionin it, mark with a 1. If ony dissolved, mark with 0
  mutate(Has_Crit_Fraction = ifelse(Crit_Fraction ==  max(Simplified_sample_fraction), 1, 0)) %>%
  # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
  ungroup() %>%
  filter((Has_Crit_Fraction == 1 & Simplified_sample_fraction == Crit_Fraction) | Has_Crit_Fraction == 0) %>% 
  # Remove results with null evaluation_criteria (indicating a mismatch between water type and criteria (ex freshwater phosporus samples ))
  filter(!is.na(crit)) %>%
  mutate(evaluation_result = ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode == 2, Result_cen*0.8, 
                                    ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode != 2, Result_cen*0.59, Result_cen ))) %>%
  mutate(violation = ifelse(evaluation_result > crit, 1, 0 ))


IR_export(tox_HH_assesment, "Parameters/Tox_HH/Data_Review/", "Tox_HH_chlordane", "data")
#write.csv(tox_HH_assesment, "Parameters/Tox_HH/Data_Review/HH_tox_analysis.csv")
  
tox_HH_categories <- tox_HH_assesment %>%
  group_by(AU_ID, Char_Name, Simplified_sample_fraction,Crit_Fraction) %>%
  summarise(OWRD_Basin = first(OWRD_Basin),
            Pollu_ID = first(Pollu_ID),
            crit = max(crit),
            num_samples = n(),
            summed_percent_nondetect = sum(Result_Operator == "<")/n(),
            num_3d = ifelse(is.na(summed_percent_nondetect), sum(Result_Operator == "<" & IRResultNWQSunit > crit ), 
                            sum(Result_Operator == "<" & Result_cen == 0)),
            num_not_3d = num_samples - num_3d,
            percent_3d = num_3d/num_samples * 100,
            num_violations = sum(violation),
            geomean = ifelse(num_not_3d >= 3, geo_mean(Result_cen[!(Result_Operator == "<" & IRResultNWQSunit > crit)]), 
                             NA))  %>%
  ungroup() %>%
  group_by(AU_ID, Char_Name) %>%
  mutate(num_fraction_types =  n(),
         IR_category =  case_when(percent_3d == 100 ~ "Cat3D",
                                  num_samples >= 3 & geomean > crit ~ "Cat5",
                                  num_samples < 3 & num_violations >= 1 ~ "Cat3B",
                                  num_samples < 3 & num_violations == 0 ~ "Cat3",
                                  num_not_3d < 3 ~ "Cat3",
                                  geomean <= crit ~ "Cat 2",
                                  TRUE ~ "ERROR"), 
         Rationale =  case_when(percent_3d == 100 ~ "All results have detection limits above criteria",
                                num_samples >= 3 & geomean > crit ~ paste("Geometric mean of", round(geomean, 7), "above criteria of", crit),
                                num_samples < 3 & num_violations >= 1 ~ paste('Only', num_samples, " samples", 'and', num_violations, "excursions"), 
                                num_samples < 3 & num_violations == 0 ~ paste('Only', num_samples, " samples", 'and', "0", "excursions"),
                                num_not_3d < 3 ~ paste("Only", num_not_3d, 'samples have QL above criteria'),
                                geomean <= crit ~ "Geometric mean < criteria",
                                TRUE ~ "ERROR"),
        geomean = ifelse(summed_percent_nondetect == 100 & !is.na(summed_percent_nondetect), NA, geomean ))  %>%
  arrange(AU_ID, Char_Name)
 

# 
# tox_HH_categories <- tox_HH_assesment %>%
#   group_by(AU_ID, Char_Name, Simplified_sample_fraction,Crit_Fraction) %>%
#   summarise(OWRD_Basin = first(OWRD_Basin),
#             Pollu_ID = first(Pollu_ID),
#             crit = max(crit),
#             num_samples = n(),
#             num_non3d_results = sum(percent_3)/n(),
#             num_3d = ifelse(is.na(summed_percent_nondetect), sum(Result_Operator == "<" & IRResultNWQSunit > crit ), 
#                             sum(summed_percent_nondetect)/100 * num_samples),
#             num_not_3d = num_samples - num_3d,
#             percent_3d = ifelse(is.na(summed_percent_nondetect),  
#                                 round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100), 
#                                 sum(summed_percent_nondetect)),
#             num_violations = sum(violation),
#             geomean = ifelse(num_not_3d >= 3, geo_mean(Result_cen[!(Result_Operator == "<" & IRResultNWQSunit > crit)]), 
#                              NA))  %>%
#   ungroup() %>%
#   group_by(AU_ID, Char_Name) %>%
#   mutate(num_fraction_types =  n(),
#          IR_category =  case_when(percent_3d == 100 ~ "Cat3D",
#                                   num_samples >= 3 & geomean > crit ~ "Cat5",
#                                   num_samples < 3 & num_violations >= 1 ~ "Cat3B",
#                                   num_samples < 3 & num_violations == 0 ~ "Cat3",
#                                   num_not_3d < 3 ~ "Cat3",
#                                   geomean <= crit ~ "Cat 2",
#                                   TRUE ~ "ERROR"), 
#          Rationale =  case_when(percent_3d == 100 ~ "All results have detection limits above criteria",
#                                 num_samples >= 3 & geomean > crit ~ paste("Geometric mean of", geomean, "above criteria of", crit),
#                                 num_samples < 3 & num_violations >= 1 ~ paste('Only', num_samples, " samples", 'and', num_violations, "excursions"), 
#                                 num_samples < 3 & num_violations == 0 ~ paste('Only', num_samples, " samples", 'and', "0", "excursions"),
#                                 num_not_3d < 3 ~ paste("Only", num_not_3d, 'samples have QL above criteria'),
#                                 geomean <= crit ~ "Geometric mean < criteria",
#                                 TRUE ~ "ERROR"),
#          geomean = ifelse(summed_percent_nondetect == 100 & !is.na(summed_percent_nondetect), NA, geomean ))  %>%
#   arrange(AU_ID, Char_Name)


#write tablehere
IR_export(tox_HH_categories, "Parameters/Tox_HH/Data_Review/", "Tox_HH_chlordane", "Categories")
}

# To do - 
#Figure out fractions piece
