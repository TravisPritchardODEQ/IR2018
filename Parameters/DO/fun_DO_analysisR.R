library(lubridate)


## year round

# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod
Results_spawndates <- Results_censored_DO %>%
  mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(ActStartD) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(ActStartD)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
         in_spawn = ifelse(ActStartD >= SpawnStart & ActStartD <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         critstart = mdy(paste0("7/1/",year(ActStartD) )),
         critend = mdy(paste0("9/30/",year(ActStartD) )),
         is.crit = ifelse(ActStartD >= critstart & ActStartD <= critend, 1, 0 ))

# Summarize available data to get a list of AU's to be analyzed using cont. data
results_cont_summary <- Results_spawndates %>%
  filter(ResultBasesName == "30DADMean") %>%
  group_by(AU_ID) %>%
  summarise(tot_30d_metrics = n(),
            crit_30d_periods = sum(is.crit)) %>%
  filter(crit_30d_periods >= 15,
         !is.na(AU_ID))


# data to be analyzed using contiuous criteria
continuous_data_analysis <- Results_spawndates %>%
  filter(AU_ID %in% results_cont_summary$AU_ID)