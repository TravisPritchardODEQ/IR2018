source("Parameters/Bacteria/fun_Bacteria_data.R")
source("Parameters/Bacteria/fun_coast_contact.R")
source("Parameters/Bacteria/fun_fresh_contact.R")
source("Parameters/Bacteria/fun_shell_harvest.R")

# Bring data into analysis
Results_censored_bacteria <- Bacteria_data("IR 2018")

# Water Contact Recreation - Freshwater
Bacteria_fresh_contact_rec <- Fresh_Contact_rec(Results_censored_bacteria)

# Water Contact recreation - Coastal Water 
Bacteria_Coast_contact_rec <- Coastal_Contact_rec(Results_censored_bacteria)

#Shellfish Harvesting
Bacteria_Shell_Harvest <- Shell_Harvest(Results_censored_bacteria)

