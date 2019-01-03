library(IRlibrary)
library(lubridate)


TOX_AL_penta_analysis <- function(df){

# Assign violations
penta_data_analysis <- df %>%
  mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(Acute_FW, Chronic_FW, na.rm = TRUE), pmin(Acute_SW, Chronic_SW, na.rm = TRUE) )) %>%
  mutate(violation = ifelse(Result_cen > evaluation_crit, 1, 0 ))


IR_export(penta_data_analysis, "Parameters/Tox_AL/Data_Review", "TOX_AL_Pentachlorophenol", "data" )



#Summarize data and assign critical excursions and IR category
penta_data_summary <- penta_data_analysis %>%
  group_by(AU_ID) %>%
  summarise(num_samples = n(),
            num_Violations = sum(violation),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > evaluation_crit )/num_samples * 100)) %>%
  mutate(critical_excursions = excursions_tox(num_samples),
         Category = ifelse(percent_3d == 100, "Cat 3D", 
                           ifelse(num_Violations > critical_excursions, "Cat 5", "Cat 2" )))

IR_export(penta_data_summary, "Parameters/Tox_AL/Data_Review", "TOX_AL_Pentachlorophenol", "categories" )

}