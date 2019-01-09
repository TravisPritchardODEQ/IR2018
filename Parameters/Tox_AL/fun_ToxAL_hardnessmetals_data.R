require(tidyverse)
require(IRlibrary) 
library(odbc)
library(DBI)
library(glue)




Hardness_based_metals <- function(database){
print("Fetch metals data from IR database")

#open connection to database
con <- DBI::dbConnect(odbc::odbc(), database)

#Build query language to get Pentachlorophenol data out. this grabs the IR 2018 db view [dbo].[VW_Pentachlorophenol]

db_qry <- glue::glue_sql( "SELECT *
  FROM [IntegratedReport].[dbo].[VW_metals]", .con = con)

# Send query to database and return with the data
Results_import <-  DBI::dbGetQuery(con, db_qry)

Results_import_no_NAs <- Results_import %>%
  filter(!is.na(MLocID))

print(paste("Returned", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations"))

#Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
mlocs <- unique(Results_import_no_NAs$MLocID)

# chr_uids for hardness based metals ancillary data
# Hardness = 1097
# Ca = 727
# mg = 1244
print("Fetch ancillary data from IR database")
ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
                                WHERE chr_uid in (1097, 727, 1244) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = con)

#Query to get ancillary data
Results_ancillary <- DBI::dbGetQuery(con, ancillary_qry)


print("Joining ancillary data")
# remove suspended fraction, if any
# Simplify Sample fraction to either Total or Dissolved
# Set new char name to incorporate sample fraction, this makes for easier to read column headers
# Remove columns that ae not needed in acillary data
# group by parameters that define single sample
# Choose only the first result for that day
# Spread the data from long format to wide format
spread <- Results_ancillary %>%
  filter(!Sample_Fraction %in% c("Suspended")) %>%
  mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction %in% c("Total Recoverable", "Acid Soluble") | is.na(Sample_Fraction), 'Total', Sample_Fraction )) %>%
  mutate(Char_Name = paste(Char_Name, "-", Simplfied_Sample_Fraction)) %>%
  select(-Sample_Fraction, -Char_Speciation, -IRWQSUnitName) %>%
  group_by(MLocID, SampleStartDate,Char_Name,Result_Depth  ) %>%
  summarise(result = first(IRResultNWQSunit)) %>%
  arrange(MLocID, SampleStartDate) %>%
  spread(key = Char_Name, value = result) 

# Make the column names a bit easier to work with 
names(spread) <- gsub(" ", "", names(spread))
names(spread) <- gsub(",|-", "_", names(spread))



# Get hardness value to be used in assessment -----------------------------

# default to using total Hardness as CaCO3, if we have it
  # Use dissolved Hardness as CaCO3, if we have it
  # If we don't have either, use total calcium and Magnesium
  # use dissolved fractions of Ca and Mg, if we don't have total
Hardness <- spread %>%
  mutate(Hardness = ifelse(!is.na(Hardness_Ca_Mg_Total), Hardness_Ca_Mg_Total, 
                                ifelse(!is.na(Hardness_Ca_Mg_Dissolved), Hardness_Ca_Mg_Dissolved, 
                                       ifelse(!is.na(Calcium_Total) & !is.na(Magnesium_Total), 2.497*Calcium_Total + 4.1189*Magnesium_Total, 
                                               ifelse(!is.na(Calcium_Total) & !is.na(Magnesium_Dissolved) & is.na(Magnesium_Total) , 2.497*Calcium_Total + 4.1189*Magnesium_Dissolved, 
                                                      ifelse(!is.na(Calcium_Dissolved) & is.na(Calcium_Total) & !is.na(Magnesium_Total), 2.497*Calcium_Dissolved + 4.1189*Magnesium_Total, 
                                                             ifelse(!is.na(Calcium_Dissolved) & is.na(Calcium_Total) & !is.na(Magnesium_Dissolved) & is.na(Magnesium_Total), 2.497*Calcium_Dissolved +4.1189*Magnesium_Dissolved, 
                                                                    NA )))))))

# Join ancillary data to metals data --------------------------------------

# Join data together 
# get default hardness values, if we don't have a measured (or calculated) hardness
# get value of hardness to use in calculating criteria
  # if hardness is > 400, set at 400
metals_hardness <- Results_import_no_NAs %>%
  left_join(Hardness, by = c('MLocID', 'SampleStartDate', 'Result_Depth')) %>%
  mutate(default_hardness = ifelse(AU_ID %in% c('REPLACE THIS', 'WITH', 'AU_IDs', "OF COLUMBIA MAIN STEM"), 48.7, 
                                   ifelse(EcoRegion3 == 11, 21.7,
                                          ifelse(EcoRegion3 == 4, 10.0, 
                                                 ifelse(EcoRegion3 == 1, 14.5, 
                                                        ifelse(EcoRegion3 == 10, 23.4, 
                                                               ifelse(EcoRegion3 == 9, 19.3, 
                                                                      ifelse(EcoRegion3 == 78, 28.5,
                                                                             ifelse(EcoRegion3 == 80, 32.3,
                                                                                    ifelse(EcoRegion3 == 12, 80.9, 
                                                                                           ifelse(EcoRegion3 == 3, 25.0, NA )))))))))),
         crit_hardness = ifelse(is.na(Hardness), default_hardness, Hardness )) %>%
  mutate(crit_hardness = ifelse(crit_hardness > 400, 400, crit_hardness ))




#Build constants table to use to join
constants <- data.frame("Char_Name" = c('Cadmium', 'Chromium', 'Lead', 'Nickel', 'Silver', 'Zinc'),
                        "ma" = c(NA, 0.8190, 1.273, 0.8460, 1.72, 0.8473),
                        "ba" = c(NA, 3.7256, -1.460, 2.255, -6.59, 0.884),
                        "mc" = c(0.7409, .08190, 1.273, 0.8460, NA, 0.8473),
                        'bc' = c(-4.719, 0.6848, -4.705, 0.0584, NA, 0.884), stringsAsFactors = FALSE)


# join metals data with constants table
# Get the CF constant value
# get criteria, for silver, if the acute criteria is lower than the chronic, use acute, else use chronic
Hardness_analysis <- metals_hardness %>%
  left_join(constants, by = "Char_Name") %>%
  mutate(CF = ifelse(Char_Name == 'Cadmium', 1.101672-  (log(crit_hardness) * 0.041838), 
                     ifelse(Char_Name == 'Chromium', 0.860, 
                            ifelse(Char_Name == 'Lead', 1.46203 - (log(crit_hardness) * 0.145712), 
                                   ifelse(Char_Name == 'Nickel', 0.997, 
                                          ifelse(Char_Name == 'Silver', 0.85, 
                                                 ifelse(Char_Name == 'Zinc', 0.986, "ERROR" ) )))))) %>%
  mutate(CF = as.numeric(CF)) %>%
  mutate(crit = ifelse(Char_Name == 'Silver', pmin(0.10, (exp(ma*(log(crit_hardness)+ba))*CF)), 
                        exp(mc*(log(crit_hardness)+bc))*CF ),
         crit = ifelse(WaterTypeCode %in% c(1,3), pmin(Chronic_SW, Acute_SW, na.rm = TRUE), crit ))


print('Begin analysis')

Results_censored <- Censored_data(Hardness_analysis, crit = `crit` ) %>%
  mutate(excursion = ifelse(IRResultNWQSunit > crit , 1, 0 ),
         Simplfied_Sample_Fraction = ifelse(Sample_Fraction ==  "Dissolved",  "Dissolved", "Total" ))

IR_export(Results_censored, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Hardness_Metals", "Data")



Results_tox_AL_HBM_cats <- Results_censored %>%
  group_by(AU_ID, Char_Name) %>%
  #Summarise data
  summarise(OWRD_Basin = first(OWRD_Basin),
            criteria_fraction = first(Crit_fraction),
            num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
            num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
            num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
            num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
            num_excursions_all = sum(excursion),
            num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
            num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
            num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction,
                                                     num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction ) ), 
            critical_excursions = excursions_tox(num_samples_crit_excursion_calc)) %>%
 # Assign categories
    mutate(IR_category = ifelse(percent_3d == 100, "Cat 3D",
                                ifelse(num_samples_crit_excursion_calc == 1 & num_excursions_all == 1, "Cat 3B",
                                      ifelse(num_samples_crit_excursion_calc == 1 & num_excursions_all == 0, "Cat 3", 
                                               ifelse(num_excursions_all > critical_excursions, "Cat 5", 
                                                      ifelse(num_excursions_all <= critical_excursions, "Cat 2", "ERROR" )))) ) )

IR_export(Results_tox_AL_HBM_cats, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Hardness_Metals", "Categories")

return(Results_tox_AL_HBM_cats)

}
  