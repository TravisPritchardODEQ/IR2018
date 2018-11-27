source("Parameters/DO/fun_DO_data.R")
#source("Parameters/Temperature/fun_temp_analysis.R")
source("Parameters/DO/fun_DO_spawn.R")



Results_censored_DO <- DO_data("IR 2018")

Spawning_analysis_list <- DO_spawning_analysis(Results_censored_DO)


# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_Spawning_continuous <- Spawning_analysis_list[[1]]
DO_Spawning_instantaneous <- Spawning_analysis_list[[2]]