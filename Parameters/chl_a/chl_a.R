source("Parameters/chl_a/fun_chla_Data.R")
source("Parameters/chl_a/fun_chla_analysis.R")


Results_censored_chla <- chla_data("IR 2018")

chla_summary <- chl_assessment(Results_censored_chla)
