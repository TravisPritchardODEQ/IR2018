library(lubridate)

fun_Tox_HH_analysis <- function(df){ 


# Need to do chlordane
#   


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
  


# Put summed data together
results_analysis <- df %>%
  filter(Pollu_ID != 153) %>%
  bind_rows(PCB_data)



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


IR_export(tox_HH_assesment, "Parameters/Tox_HH/Data_Review/", "Tox_HH", "data")
#write.csv(tox_HH_assesment, "Parameters/Tox_HH/Data_Review/HH_tox_analysis.csv")
  
tox_HH_categories <- tox_HH_assesment %>%
  group_by(AU_ID, Char_Name, Simplified_sample_fraction,Crit_Fraction) %>%
  summarise(OWRD_Basin = first(OWRD_Basin),
            Pollu_ID = first(Pollu_ID),
            crit = max(crit),
            num_samples = n(),
            summed_percent_nondetect = sum(summed_percent_nondetect)/n(),
            percent_3d = ifelse(is.na(summed_percent_nondetect),  
                                round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100), 
                                sum(summed_percent_nondetect)) ,
            num_violations = sum(violation),
            geomean = geo_mean(Result_cen)) %>%
  ungroup() %>%
  group_by(AU_ID, Char_Name) %>%
  mutate(num_fraction_types =  n(),
         IR_category = ifelse(percent_3d == 100, "Cat3D", 
                              ifelse(num_samples >= 3 & geomean > crit, "Cat5", 
                              ifelse(num_samples < 3 & num_violations >= 1, "Cat3B", 
                                     ifelse(num_samples < 3 & num_violations == 0, "Cat3", "Cat2" )))),
         geomean = ifelse(summed_percent_nondetect == 100 & !is.na(summed_percent_nondetect), NA, geomean ))  %>%
  arrange(AU_ID, Char_Name)
 
#write tablehere
IR_export(tox_HH_categories, "Parameters/Tox_HH/Data_Review/", "Tox_HH", "Categories")
}

# To do - 
#Figure out fractions piece
