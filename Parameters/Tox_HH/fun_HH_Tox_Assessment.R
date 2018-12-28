library(lubridate)

fun_Tox_HH_analysis <- function(df){ 


DDT <- Results_censored_tox_HH %>%
  filter(Pollu_ID %in% c(48, 49, 50 ))
  
  
tox_HH_assesment <- df %>%
  # Set null crit_fraction to "Total"
  mutate(Crit_Fraction = ifelse(is.na(Crit_Fraction), "Total", Crit_Fraction )) %>%
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



write.csv(tox_HH_assesment, "Parameters/Tox_HH/Data_Review/HH_tox_analysis.csv")
  
tox_HH_categories <- tox_HH_assesment %>%
  group_by(AU_ID, Char_Name, Simplified_sample_fraction,Crit_Fraction) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            crit = max(crit),
            num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
            num_violations = sum(violation),
            geomean = geo_mean(Result_cen)) %>%
  ungroup() %>%
  group_by(AU_ID, Char_Name) %>%
  mutate(num_fraction_types =  n(),
         IR_category = ifelse(percent_3d == 100, "Cat3D", 
                              ifelse(num_samples >= 3 & geomean >= crit, "Cat5", 
                              ifelse(num_samples < 3 & num_violations >= 1, "Cat3B", 
                                     ifelse(num_samples < 3 & num_violations == 0, "Cat3", "Cat2" )))) )  %>%
  arrange(AU_ID, Char_Name)
 
#write tablehere
}

# To do - 
#Figure out fractions piece
# make sure units are correct in data pull