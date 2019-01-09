source("Parameters/Bacteria/fun_Bacteria_data.R")
source("Parameters/Bacteria/fun_Bacteria_data_censored.R")
source("Parameters/Bacteria/fun_coast_contact.R")
source("Parameters/Bacteria/fun_fresh_contact.R")
source("Parameters/Bacteria/fun_shell_harvest.R")


# Data Prep ---------------------------------------------------------------


# Bring data into analysis
Bacteria_results <- Bacteria_data("IR 2018")

# Save valid results as RData object
save(Bacteria_results, file = "Parameters/Bacteria/bacteria_valid_data.RData")

#######################################################################################################
###                         Stop here and review the invalid data file.                             ###
###                      For valid data, mark the Conclusion field as Valid                         ###
###                             Reformat the MLocID and time columns                                ###
#######################################################################################################

#Load calid results back into the Environment
load("Parameters/Bacteria/bacteria_valid_data.RData")

#Reinput the data after the manual data validation step
Validated_reults <- IR_Validation_Import(file = "Parameters/Invalid_data/Invalid-Bacteria.csv", df = Bacteria_results)

# Perfrom the data censoring and cleaning
Results_censored_bacteria <- Bacteria_data_censored(Validated_reults)


# Data analysis -----------------------------------------------------------


# Water Contact Recreation - Freshwater
Bacteria_fresh_contact_rec <- Fresh_Contact_rec(Results_censored_bacteria)

IR_export(Bacteria_fresh_contact_rec, "Parameters/Bacteria/Data Review", "Bacteria_Fresh_Contact", "Categories" )

# Water Contact recreation - Coastal Water 
Bacteria_Coast_contact_rec <- Coastal_Contact_rec(Results_censored_bacteria)

#Shellfish Harvesting
Bacteria_Shell_Harvest <- Shell_Harvest(Results_censored_bacteria)

IR_export(Bacteria_Shell_Harvest, "Parameters/Bacteria/Data Review", "Bacteria_Shell_Harvest", "Categories" )
