
DO_year_round_analysis <- function(df){

library(lubridate)



# Year round --------------------------------------------------------------


# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod
Results_spawndates <- df %>%
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



# Continuous criteria analysis --------------------------------------------

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
cont_perc_sat_check <- continuous_data_analysis %>%
  filter(AU_ID %in% unique(subset(continuous_data_categories, category == "Check percent Sat" )$AU_ID) )

continuous_mon_locs <- unique(cont_perc_sat_check$MLocID)


# Get data from database --------------------------------------------------


# Get DO and temp data from IR_database to calculate percent sat --------

Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (ResultBasesName = 'Mean') AND MLocID in ({continuous_mon_locs*})"

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

Doqry <- glue::glue_sql(Doqry, .con = con)

perc_sat_DO <- DBI::dbGetQuery(con, Doqry)

tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (ResultBasesName = 'Mean') AND MLocID in ({continuous_mon_locs*})"

tempqry <- glue::glue_sql(tempqry, .con = con)

perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)

DBI::dbDisconnect(con)



# Join --------------------------------------------------------------------


perc_sat_temp_join <- perc_sat_temp %>%
  select(MLocID, Result4IR, ActStartD, ActStartT, ResultBasesName) %>%
  rename(Temp_res = Result4IR)

DO_sat <- perc_sat_DO %>%
  rename(DO_res =  Result4IR) %>%
  left_join(perc_sat_temp_join, by = c('MLocID', 'ActStartD', 'ActStartT', 'ResultBasesName')) %>%
  mutate(DO_sat = DOSat_calc(DO_res, Temp_res, ELEV_Ft ),
         ma.DOS.mean30 = "")

# calculate 30-D averages

#Set loop for each monitoring location

#prep list
monloc_do_list <- list()

for(i in 1:length(unique(DO_sat$MLocID))){
  
  print(paste("Station", i, "of", length(unique(DO_sat$MLocID))))
  
  station = unique(DO_sat$MLocID)[i]
  
  #Filter dataset to only look at 1 monitoring location at a time
  daydat_station <- DO_sat %>%
    filter(MLocID == station) %>%
    mutate(startdate30 = as.Date(ActStartD) -30) %>%
    arrange(ActStartD)
  
 # Begin 30-d moving averages
   print("Begin 30 day moving averages" )
  pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
  for(l in 1:nrow(daydat_station)){
    
    
    start30 <- daydat_station$startdate30[l]
    end30 <- daydat_station$ActStartD[l] 
    
    station_30day <- daydat_station %>%
      filter(ActStartD <= end30 & ActStartD >= start30) 
    
    ma.mean30 <- ifelse(length(unique(station_30day$ActStartD)) >= 29, mean(station_30day$DO_sat), NA )
    
    
    daydat_station[l,"ma.DOS.mean30"] <- ifelse(l >= 30, round(ma.mean30, 2), NA)
    setTxtProgressBar(pb, l)
  } #end of 30day loop
  
  # Assign dataset filtered to 1 monitoring location to a list for combining outside of for loop
  monloc_do_list[[i]] <- daydat_station
  
}

  
# Bind rows to get DO_sat averages

DO_sat_avgs <-  bind_rows(monloc_do_list)  


# Join DOsat to 30_D metrics -----------------------------------------------
DO_sat_join <- DO_sat_avgs %>%
  mutate(ResultBasesName = "30DADMean",
         Date = as.Date(ActStartD)) %>%
  select(MLocID, ma.DOS.mean30, Date,ResultBasesName) 


yr_round_cont_DO_data_analysis <- continuous_data_analysis %>%
  mutate(Date = as.Date(ActStartD)) %>%
  left_join(DO_sat_join, by = c('MLocID', 'Date', 'ResultBasesName')) %>%
  mutate(Violation = ifelse(DO_Class == "Cold Water"& ResultBasesName == "30DADMean" & Result4IR < crit_30D & ma.DOS.mean30 < 90, 1,
                            ifelse(DO_Class != "Cold Water"& ResultBasesName == "30DADMean" & Result4IR < crit_30D, 1, 
                              ifelse(ResultBasesName == "7DADMin" & Result4IR < crit_7Mi, 1, 
                                   ifelse(ResultBasesName == "Minimum" & Result4IR < crit_Min, 1, 0 ))))) 

write.csv(yr_round_cont_DO_data_analysis, file = "Parameters/DO/Data Review/yearround_continuous_data_analysis.csv", row.names = FALSE)


yr_round_cont_data_categories <- continuous_data_analysis %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(Total_violations = sum(Violation),
            Sum_30D_violations = sum(Violation [ResultBasesName == "30DADMean"]),
            Sum_7mi_violations = sum(Violation [ResultBasesName == "7DADMin"]),
            Sum_abs_min_violations = sum(Violation [ResultBasesName == "Minimum"])) %>%
  mutate(category = ifelse(Sum_30D_violations >= 2 |
                                Sum_7mi_violations >= 2 |
                                Sum_abs_min_violations >= 2, "Cat 5", 
                             ifelse(Sum_30D_violations < 2 &
                                                  Sum_7mi_violations < 2 &
                                                  Sum_abs_min_violations < 2, "Cat 2", "Error" )))




# Insantaneous metrics ----------------------------------------------------

instant_data_analysis <- Results_spawndates %>%
  filter(!AU_ID %in% results_cont_summary$AU_ID) %>%
  filter(ResultBasesName %in% c("Minimum", NA)) %>%
  mutate(Violation_crit = ifelse(Result4IR < crit_30D, 1, 0 ))

instant_data_categories <- instant_data_analysis %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(num_samples = n(),
            num_critical_samples = sum(is.crit),
            num_below_crit = sum(Violation_crit)) %>%
  mutate(critical_excursions = excursions_conv(num_samples)) %>%
  mutate(category = ifelse(num_critical_samples < 10 & 
                             num_below_crit > 0, "Cat 3B", 
                           ifelse(num_critical_samples < 10 & 
                                    num_below_crit == 0, "Cat 3", 
                                  ifelse(num_critical_samples >= 10 &
                                           num_below_crit > critical_excursions &
                                           DO_Class != "Cold Water", "Cat 5", 
                                         ifelse(num_critical_samples >= 10 &
                                                   num_below_crit > critical_excursions &
                                                   DO_Class == "Cold Water", "Check percent Sat",
                                                ifelse(num_critical_samples >= 10 &
                                                         num_below_crit <= critical_excursions, "Cat 2", "ERROR" ))))))


# Data to be used to check percent saturation
inst_perc_sat_check <- instant_data_analysis %>%
  filter(AU_ID %in% unique(subset(instant_data_categories, category == "Check percent Sat" )$AU_ID) ) 

instant_mon_locs <- unique(inst_perc_sat_check$MLocID)
  

# Get data from database --------------------------------------------------


# Get DO and temp data from IR_database to calculate percent sat --------

Doqry <- "SELECT * 
FROM            VW_DO
WHERE        ((ResultBasesName = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((ResultBasesName IS NULL) AND MLocID in ({instant_mon_locs*}))"

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

Doqry <- glue::glue_sql(Doqry, .con = con)

instant_perc_sat_DO <- DBI::dbGetQuery(con, Doqry)

tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        ((ResultBasesName = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((ResultBasesName IS NULL) AND MLocID in ({instant_mon_locs*}))"

tempqry <- glue::glue_sql(tempqry, .con = con)

instant_perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)

DBI::dbDisconnect(con)

#Join


# Pare down temp table to be used for joining
instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
  select(MLocID, ResultBasesName, Result4IR, ActStartD, ActStartT, ActDepth) %>%
  rename(Temp_res = Result4IR)


# Join DO and temp tables and calculate DO-Sat
instant_DO_sat <- instant_perc_sat_DO %>%
  rename(DO_res =  Result4IR) %>%
  left_join(instant_perc_sat_temp_join, by = c('MLocID', 'ActStartD', 'ActStartT', 'ResultBasesName', 'ActDepth')) %>%
  mutate(DO_sat = DOSat_calc(DO_res, Temp_res, ELEV_Ft ))  %>%
  select(MLocID, ActStartD, ActStartT, ResultBasesName, ActDepth,DO_sat ) %>%
  mutate(ActDepth = as.numeric(ActDepth))


#Join back in

Instant_data_analysis_DOS <- Results_spawndates %>%
  filter(!AU_ID %in% results_cont_summary$AU_ID) %>%
  filter(ResultBasesName %in% c("Minimum", NA)) %>%
  left_join(instant_DO_sat, by = c('MLocID', 'ActStartD', 'ActStartT', 'ResultBasesName', 'ActDepth')) %>%
  mutate(Violation = ifelse(DO_Class == "Cold Water" & 
                              Result4IR < crit_30D &
                              DO_sat < 90.0, 1, 
                            ifelse(DO_Class != "Cold Water" & 
                                     Result4IR < crit_30D, 1, 0))  )

write.csv(Instant_data_analysis_DOS, file = "Parameters/DO/Data Review/yearround_instantaneous_data_analysis.csv", row.names = FALSE)

yr_round_instant_categories <- Instant_data_analysis_DOS %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(num_samples = n(),
            num_critical_samples = sum(is.crit),
            num_below_crit = sum(Violation, na.rm = TRUE)) %>%
  mutate(critical_excursions = excursions_conv(num_samples)) %>%
  mutate(category = ifelse(num_critical_samples < 10 & 
                             num_below_crit > 0, "Cat 3B", 
                           ifelse(num_critical_samples < 10 & 
                                    num_below_crit == 0, "Cat 3", 
                                  ifelse(num_critical_samples >= 10 &
                                           num_below_crit > critical_excursions , "Cat 5", 
                                                 ifelse(num_critical_samples >= 10 &
                                                         num_below_crit <= critical_excursions, "Cat 2", "ERROR" )))))

return(list(yr_round_cont_data_categories,yr_round_instant_categories ))

}

  