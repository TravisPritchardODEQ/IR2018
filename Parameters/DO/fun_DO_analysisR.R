library(lubridate)



# Year round --------------------------------------------------------------


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


# initial contiuous criteria analysis
# filter down to AUs that are to be evaluated with cont metrics
# Filter down to only 30-D, 7-Mi, and daily minimums
# Flag various violations
continuous_data_analysis <- Results_spawndates %>%
  filter(AU_ID %in% results_cont_summary$AU_ID) %>%
  filter(ResultBasesName %in% c("30DADMean", "7DADMin", "Minimum")) %>%
  mutate(Violation = ifelse(ResultBasesName == "30DADMean" & Result4IR < crit_30D, 1, 
                            ifelse(ResultBasesName == "7DADMin" & Result4IR < crit_7Mi, 1, 
                                   ifelse(ResultBasesName == "Minimum" & Result4IR < crit_Min, 1, 0 )))) 

continuous_data_categories <- continuous_data_analysis %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(Total_violations = sum(Violation),
            Sum_30D_violations = sum(Violation [ResultBasesName == "30DADMean"]),
            Sum_7mi_violations = sum(Violation [ResultBasesName == "7DADMin"]),
            Sum_abs_min_violations = sum(Violation [ResultBasesName == "Minimum"])) %>%
  mutate(category = ifelse(DO_Class != "Cold Water" & 
                             (Sum_30D_violations >= 2 |
                                Sum_7mi_violations >= 2 |
                                Sum_abs_min_violations >= 2), "Cat 5", 
                           ifelse(DO_Class == "Cold Water" & 
                                    (Sum_7mi_violations >= 2 |
                                       Sum_abs_min_violations >= 2), "Cat 5", 
                                 ifelse(DO_Class == "Cold Water" &
                                           Sum_30D_violations >= 2 &
                                           Sum_7mi_violations < 2 &
                                           Sum_abs_min_violations < 2, "Check percent Sat",  
                                         ifelse(Sum_30D_violations < 2 &
                                                  Sum_7mi_violations < 2 &
                                                  Sum_abs_min_violations < 2, "Cat 2", "Error" )))))


# Data to be used to check percent saturation
perc_sat_check <- continuous_data_analysis %>%
  filter(AU_ID %in% unique(subset(continuous_data_categories, category == "Check percent Sat" )$AU_ID) )
  
  
  