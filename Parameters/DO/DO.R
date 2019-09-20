library(IRlibrary)
source("Parameters/DO/fun_DO_data.R")
source("Parameters/DO/fun_DO_yearround.R")
source("Parameters/DO/fun_DO_spawn.R")
source("Parameters/DO/fun_DO_estuary.R")



DO_results <-  DO_data("IR 2018")
# 
# #######################################################################################################
# ###                         Stop here and review the invalid data file.                             ###
# ###                      For valid data, mark the Conclusion field as Valid                         ###
# ###                             Reformat the MLoc and time columns                                  ###
# #######################################################################################################
# 
# #Reinput the data after the manual data validation step
Validated_results <- IR_Validation_Import(file = "Parameters/Invalid_data/Invalid-DO.csv", df = DO_results)

#save(Validated_results, file = "Parameters/DO/Validated_results.Rdata" )
#as of 8/12/19
# load("Parameters/DO/Validated_results.Rdata")

Yearround_analysis_list <-  DO_year_round_analysis(Validated_results)
# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_yearround_continuous <- Yearround_analysis_list[[1]]
DO_yearround_instantaneous <- Yearround_analysis_list[[2]]


IR_export(DO_yearround_continuous, "Parameters/DO/Data_Review", "DO_yearround_continuous", "categories")
IR_export(DO_yearround_instantaneous, "Parameters/DO/Data_Review", "DO_yearround_instantaneous", "categories")


Spawning_analysis_list <- DO_spawning_analysis(Validated_results)

# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_Spawning_continuous <- Spawning_analysis_list[[1]]
DO_Spawning_instantaneous <- Spawning_analysis_list[[2]]

IR_export(DO_Spawning_continuous, "Parameters/DO/Data_Review", "DO_Spawning_continuous", "categories")
IR_export(DO_Spawning_instantaneous, "Parameters/DO/Data_Review", "DO_Spawning_instantaneous", "categories")


estuary_list <- DO_estuary_analysis(Validated_results)
estuary_spawn <- DO_estuary_spawn(Validated_results)

