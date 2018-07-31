source("Parameters/pH/fun_pH_data.R")
source("Parameters/pH/fun_pH_assessment.R")


Results_censored <- pH_data("A:/Integrated_Report/IR_Database/IR_2018.accdb")
pH_summary <-  pH_assessment(Results_censored)
