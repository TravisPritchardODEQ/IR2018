source("Parameters/Temperature/fun_Temp_data.R")
source("Parameters/Temperature/fun_temp_analysis.R")



Results_censored_temp <- Temp_data("IR 2018")

temperature_summary <- temp_asessment(Results_censored_temp)

