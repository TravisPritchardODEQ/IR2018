database <- 'IR 2018'

print("Fetch Ammonia data from IR database")

#open connection to database
con <- DBI::dbConnect(odbc::odbc(), database)

#Build query language to get Pentachlorophenol data out. this grabs the IR 2018 db view [dbo].[VW_Pentachlorophenol]

db_qry <- glue::glue_sql( "SELECT *
                          FROM [IntegratedReport].[dbo].[VW_Ammonia_AL]", .con = con)

# Send query to database and return with the data
Results_import <-  DBI::dbGetQuery(con, db_qry)

Results_import_no_NAs <- Results_import %>%
  filter(!is.na(MLocID))

print(paste("Returned", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations"))

#Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
mlocs <- unique(Results_import_no_NAs$MLocID)

# chr_uids for ammonia ancillary data
# Temp = 2849
# pH = 1648

print("Fetch ancillary data from IR database")
ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                FROM [IntegratedReport].[dbo].[InputRaw]
                                WHERE chr_uid in (2849, 1648) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = con)



#Query to get ancillary data
Results_ancillary <- DBI::dbGetQuery(con, ancillary_qry)


print("Joining ancillary data")

# Spread the data from long format to wide format
spread <- Results_ancillary %>%
  filter(!Sample_Fraction %in% c("Suspended")) %>%
  group_by(MLocID, SampleStartDate,Char_Name,Result_Depth  ) %>%
  summarise(result = first(IRResultNWQSunit)) %>%
  arrange(MLocID, SampleStartDate) %>%
  spread(key = Char_Name, value = result) %>%
  rename(Temp = "Temperature, water") %>%
  mutate(pH = ifelse(pH < 6.5, 6.5, 
                     ifelse(pH > 9.0, 9.0, pH )))


# Join table together
ammonia_data <- Results_import %>%
  left_join(spread, by = c('MLocID', 'SampleStartDate', 'Result_Depth')) %>%
  filter(!is.na(pH) & !is.na(Temp)) %>%
  mutate(crit = pmin((0.275/(1 + 10 ^ (7.204 - pH))) + (39.0/ (1 + 10 ^(pH - 7.204 ))) , 
                    0.7249 * ((0.0114/ ( 1 + 10^(7.204 - pH))) + (1.6181 / (1 + 10 ^ (pH - 7.204))) * (23.12*10^(0.036*(20-Temp)))))
         )

Results_censored <- Censored_data(ammonia_data, crit = `crit` ) %>%
  mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction %in% c("Dissolved", "Filterable"),  "Dissolved", "Total" )) %>%
  group_by(MLocID, SampleStartDate, SampleStartTime ,Char_Name,Result_Depth ) %>%
  mutate(has_total = ifelse(max(Simplfied_Sample_Fraction) == "Total", 1, 0 )) %>%
  ungroup() %>%
  filter((has_total == 1 & Simplfied_Sample_Fraction == "Total") |
           (has_total == 0 & Simplfied_Sample_Fraction == "Dissolved") ) %>%
  mutate(excursion = ifelse(Result_cen > crit, 1, 0 ))

IR_export(Results_censored, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Ammonia", "Data")

Results_tox_Ammonia_categories <- Results_censored %>%
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
                                                          ifelse(num_excursions_all <= critical_excursions, "Cat 2", "ERROR" ))))  )
         )

