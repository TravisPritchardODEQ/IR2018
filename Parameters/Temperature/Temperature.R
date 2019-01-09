source("Parameters/Temperature/fun_Temp_data.R")
source("Parameters/Temperature/fun_Temp_data_narrative.R")
source("Parameters/Temperature/fun_temp_analysis.R")



# Data prep ---------------------------------------------------------------



Results_censored_temp <- Temp_data("IR 2018")


#######################################################################################################
###                         Stop here and review the invalid data file.                             ###
###                      For valid data, mark the Conclusion field as Valid                         ###
###                             Reformat the MonLoc and time columns                                    ###
#######################################################################################################

Validated_results <- IR_Validation_Import(file = "Parameters/Invalid_data/Invalid-Temperature.csv", df = Results_censored_temp)



# Data analysis -----------------------------------------------------------

temperature_summary <- temp_asessment(Results_censored_temp)
IR_export(temperature_summary, "Parameters/Temperature/Data_Review", "Temperature", "categorization" )



Temperature_narrative_std <- Temp_data_narrative("IR 2018")
IR_export(Temperature_narrative_std, "Parameters/Temperature/Data_Review", "Temperature - narrative", "data" )

