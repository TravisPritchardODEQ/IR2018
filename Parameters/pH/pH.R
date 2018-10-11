source("Parameters/pH/fun_pH_data.R")
source("Parameters/pH/fun_pH_assessment.R")


Results_censored_pH <- pH_data("IR 2018")
pH_summary <-  pH_assessment(Results_censored_pH)
