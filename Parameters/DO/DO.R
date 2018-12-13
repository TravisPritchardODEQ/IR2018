library(IRlibrary)
source("Parameters/DO/fun_DO_data.R")
source("Parameters/DO/fun_DO_yearround.R")
source("Parameters/DO/fun_DO_spawn.R")



Results_censored_DO <- DO_data("IR 2018")


Yearround_analysis_list <-  DO_year_round_analysis(Results_censored_DO)
# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_yearround_continuous <- Yearround_analysis_list[[1]]
DO_yearround_instantaneous <- Yearround_analysis_list[[2]]


IR_export(DO_yearround_continuous, "Parameters/DO", "DO_yearround_continuous")
IR_export(DO_yearround_instantaneous, "Parameters/DO", "DO_yearround_instantaneous")


Spawning_analysis_list <- DO_spawning_analysis(Results_censored_DO)

# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_Spawning_continuous <- Spawning_analysis_list[[1]]
DO_Spawning_instantaneous <- Spawning_analysis_list[[2]]

IR_export(DO_Spawning_continuous, "Parameters/DO", "DO_Spawning_continuous")
IR_export(DO_Spawning_instantaneous, "Parameters/DO", "DO_Spawning_instantaneous")

