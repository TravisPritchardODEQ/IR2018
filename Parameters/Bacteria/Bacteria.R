source("Parameters/Bacteria/fun_Bacteria_data.R")
source("Parameters/Bacteria/fun_coast_contact.R")
source("Parameters/Bacteria/fun_fresh_contact.R")


# Bring data into analysis
Results_censored <- Bacteria_data("A:/Integrated_Report/IR_Database/IR_2018.accdb")

# Water Contact Recreation - Freshwater
fresh_contact_rec <- Fresh_Contact_rec(Results_censored)

# Water Contact recreation - Coastal Water 
Coast_contact_rec <- Coastal_Contact_rec(Results_censored)

