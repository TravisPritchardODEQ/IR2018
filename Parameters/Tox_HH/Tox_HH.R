source("Parameters/Tox_HH/fun_HH_Tox_data.R")
source("Parameters/Tox_HH/fun_HH_Tox_Assessment.R")
source("Parameters/Tox_HH/fun_HH_Tox_me_hg_data.R")
source("Parameters/Tox_HH/hg_analysis.R")

options(scipen=999)


Results_censored_tox_HH <- HH_tox_data("IR 2018")

ToxHH_summary <- fun_Tox_HH_analysis(Results_censored_tox_HH)



# Mercury Tissue data -----------------------------------------------------

Censored_tissue_data <- HH_tox_hg_tissue_data("IR 2018")

TOXHH_hg_me_summary <- fun_Tox_HH_tissue_hg_analysis(Censored_tissue_data)
