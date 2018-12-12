source("Parameters/DO/fun_DO_data.R")
source("Parameters/DO/fun_DO_yearround.R")
source("Parameters/DO/fun_DO_spawn.R")



Results_censored_DO <- DO_data("IR 2018")


Yearround_analysis_list <-  DO_year_round_analysis(Results_censored_DO)
# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_yearround_continuous <- Yearround_analysis_list[[1]]
DO_yearround_instantaneous <- Yearround_analysis_list[[2]]

# write.csv(DO_yearround_continuous, "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Test data for Review/DO/DO_yearround_continuous.csv")
# write.csv(DO_yearround_instantaneous, "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Test data for Review/DO/DO_yearround_instantaneous.csv")


Spawning_analysis_list <- DO_spawning_analysis(Results_censored_DO)

# Because we can't get two values from function, they are put in list
# and need to be extracted
DO_Spawning_continuous <- Spawning_analysis_list[[1]]
DO_Spawning_instantaneous <- Spawning_analysis_list[[2]]


# write.csv(DO_Spawning_continuous, "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Test data for Review/DO/DO_Spawning_continuous.csv")
# write.csv(DO_Spawning_instantaneous, "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Test data for Review/DO/DO_Spawning_instantaneous.csv")
# 
