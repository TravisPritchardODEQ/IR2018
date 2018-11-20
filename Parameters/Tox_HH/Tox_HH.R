source("Parameters/Tox_HH/fun_HH_Tox_data.R")
source("Parameters/Tox_HH/fun_HH_Tox_Assessment.R")

options(scipen=999)


Results_censored_tox_HH <- HH_tox_data("IR 2018")

ToxHH_summary <- fun_Tox_HH_analysis(Results_censored_tox_HH)
