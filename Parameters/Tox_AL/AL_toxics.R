source("parameters/Tox_AL/fun_ToxAL_data.R")
source("parameters/Tox_AL/fun_ToxAL_analysis.R")
source("parameters/Tox_AL/fun_ToxAL_pentachlorophenol_data.R")
source("parameters/Tox_AL/fun_ToxAL_pentachlorophenol_analysis.R")
source("parameters/Tox_AL/fun_ToxAL_copper_data.R")



# Non calculated standards ------------------------------------------------

# Fetch data for analysing the parameters with non-calculated standards
Tox_AL_Censored_data <- tox_AL_data("IR 2018")

# Run the analysis
Tox_AL_IR_categories <- TOX_AL_analysis(Tox_AL_Censored_data)


# Copper ------------------------------------------------------------------

# Query copper data from IR database and join with ancillary data. Creates csv file of data
# that needs to be run through the BLM process
Copper_data("IR 2018")


# Pentachlorophenol -------------------------------------------------------

# Fetch Pentachlorophenol data
Penta_data <- Pentachlorophenol_data("IR 2018")

# Run the Pentachlorophenol analysis
Penta_analysis <- TOX_AL_penta_analysis(Penta_data)



# To do
# HArndess based metals
# Chlordane
#alkalinity