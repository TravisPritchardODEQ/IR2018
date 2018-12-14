
Bacteria_data_censored <- function(Results_valid){

# Get all the standards to be used when dealing with the censored data
Results_crit <- Results_valid %>%
  # Get lowest criteria value to set censored results
  mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))



print("Modify censored data")

#run the censored data function to set censored data. This will use the lowest crit value from above
Results_censored <- Censored_data(Results_crit, crit = `lowest_crit` ) %>%
  mutate(Result_cen = as.numeric(Result_cen))

print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))

Results_censored <- Results_censored %>%
  filter(!is.na(Result_cen))

print("Data fetch and censored data modifications complete")

return(Results_censored)

}