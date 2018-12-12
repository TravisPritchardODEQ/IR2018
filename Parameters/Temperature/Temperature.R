source("Parameters/Temperature/fun_Temp_data.R")
source("Parameters/Temperature/fun_Temp_data_narrative.R")
source("Parameters/Temperature/fun_temp_analysis.R")



Results_censored_temp <- Temp_data("IR 2018")

temperature_summary <- temp_asessment(Results_censored_temp)

write.csv(temperature_summary, '//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Test data for Review/Temperature/temperature_categories.csv')


Temperature_narrative_std <- Temp_data_narrative("IR 2018")
write.csv(Temperature_narrative_std, '//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Test data for Review/Temperature/temperature_narrative_crit.csv')
