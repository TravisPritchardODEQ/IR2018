source("Parameters/pH/fun_pH_data.R")
source("Parameters/pH/fun_pH_assessment.R")


pH_results <- pH_data("IR 2018")


#######################################################################################################
###                         Stop here and review the invalid data file.                             ###
###                      For valid data, mark the Conclusion field as Valid                         ###
#######################################################################################################

#Reinput the data after the manual data validation step
Validated_results <- IR_Validation_Import(file = "Parameters/Invalid_data/Invalid-pH.csv", df = pH_results)


pH_summary <-  pH_assessment(Validated_results)

IR_export(pH_summary, "Parameters/pH/Data_Review", "pH", "categories" )
