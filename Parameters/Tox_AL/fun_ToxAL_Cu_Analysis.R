require(tidyverse)
require(IRlibrary) 
library(odbc)
library(DBI)
library(glue)


Cu_BLM <- read.csv("E:\\IR2018\\Parameters\\Tox_AL\\Data_Review\\Copper_criteria_results.csv")

# get criteroa values - FW crit = lowest 

Cu_BLM_crit <- Cu_BLM %>%
  #lowest SW criteria 
  mutate(CCC_SW = 3.1) %>%
  mutate(crit = ifelse(WaterTypeCode == 2, pmin(CCC_ug.L,CMC_ug.L), 
                       ifelse(WaterTypeCode == 3, CCC_SW, 
                              ifelse(WaterTypeCode == 1, pmin(CCC_ug.L,CMC_ug.L,CCC_SW), "-9999")))) %>%
  mutate(crit = as.numeric(crit)) %>%
  mutate(Crit_fraction = "Dissolved")

print('Begin analysis')

# Do the data censoring and deal with samples with both total and dissolved on same date and time
# Group by single sample event (mloc, date, time, char, and depth)
# Flag if the group contains a dissolved fraction sample
# Keep only results that have a dissolved fraction and are dissolved, or keep total if
#group does not have a dissolved (where where have dissolved, remove total fractions)
# Use the conversion factor to transform total results to dissolved results  
Results_censored <- Censored_data(Cu_BLM_crit, crit = `crit` ) %>%
  mutate(Result_cen = as.numeric(Result_cen)) %>%
  mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction ==  "Dissolved",  "Dissolved", "Total" )) %>%
  group_by(MLocID, SampleStartDate, Char_Name,Result_Depth) %>%
  mutate(Has_Crit_Fraction = ifelse(Crit_fraction == "Total" & max(Simplfied_Sample_Fraction) == "Total", 1, 
                                    ifelse(Crit_fraction == "Dissolved" & min(Simplfied_Sample_Fraction) == "Dissolved", 1, 0 ))) %>%
  # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
  ungroup() %>%
  filter((Has_Crit_Fraction == 1 & Simplfied_Sample_Fraction == Crit_fraction) | Has_Crit_Fraction == 0) %>%
  mutate(excursion = ifelse(Result_cen > crit , 1, 0 ))

# If total sammple is less than the dissolved crit, it is considered valid to determine attainment. If total recoverable
# is greater than a dissolved crit, 3b may be assigned if there are no other dissolved samples to indicate impairment

IR_export(Results_censored, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Cu", "Data")



Results_tox_AL_Cu_cats <- Results_censored %>%
  group_by(AU_ID) %>%
  #Summarise data
  summarise(OWRD_Basin = first(OWRD_Basin),
            criteria_fraction = first(Crit_fraction),
            num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
            num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
            num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
            num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
            num_excursions_all = sum(excursion),
            num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
            num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
            num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction, 
                                                     num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction )),
            critical_excursions = excursions_tox(num_samples_crit_excursion_calc)) %>%
  # Assign categories
  mutate(IR_category = ifelse(percent_3d == 100, "Cat 3D",
                              ifelse((num_samples == 1 & num_excursions_all == 1) |
                                       (criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0), "Cat 3B",
                                     ifelse(num_samples_crit_excursion_calc <= 1 & num_excursions_all == 0, "Cat 3", 
                                            ifelse((criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction > critical_excursions) |
                                                     (criteria_fraction == "Total" & num_excursions_all > critical_excursions), "Cat 5", 
                                                   ifelse(num_excursions_dissolved_fraction <= critical_excursions, "Cat 2", "ERROR" )))) ) )

IR_export(Results_tox_AL_HBM_cats, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Hardness_Metals", "Categories")

return(Results_tox_AL_HBM_cats)

