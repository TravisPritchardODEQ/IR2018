
DO_spawning_analysis <- function(df){


library(lubridate)
library(odbc)
library(glue)
library(DBI)
library(zoo)



# Spawning --------------------------------------------------------------






# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod
Results_spawndates <- df %>%
  mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(ActStartD) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(ActStartD)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
         in_spawn = ifelse(ActStartD >= SpawnStart & ActStartD <= SpawnEnd & !is.na(SpawnStart), 1, 0 )) %>%
  filter(in_spawn == 1, !is.na(AU_ID))



# Continuous --------------------------------------------------------------


Results_spawndates_counts <- Results_spawndates %>%
  group_by(AU_ID, ResultBasesName) %>%
  summarise(count = n())

continuous_data_AUs <- Results_spawndates_counts %>%
  filter(ResultBasesName == "7DADMean",
         count >= 15)

continuous_data <- Results_spawndates %>%
  filter(AU_ID %in% unique(continuous_data_AUs$AU_ID),
         ResultBasesName == "7DADMean" )

continuous_mon_locs <- unique(continuous_data$MLocID)


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



# Join Do and temp and calculate DOSat ------------------------------------


perc_sat_temp_join <- perc_sat_temp %>%
  select(MLocID, Result4IR, ActStartD, ActStartT) %>%
  rename(Temp_res = Result4IR)

DO_sat <- perc_sat_DO %>%
  rename(DO_res =  Result4IR) %>%
  left_join(perc_sat_temp_join, by = c('MLocID', 'ActStartD', 'ActStartT')) %>%
  mutate(DO_sat = DOSat_calc(DO_res, Temp_res, ELEV_Ft ))

DO_sat_7dma <- DO_sat %>%
  mutate(Date = as.Date(ActStartD)) %>%
  arrange(MLocID, ActStartD) %>%
  group_by(MLocID) %>%
  mutate(startdate7 = lag(Date, 6, order_by = Date),
       # flag out which result gets a moving average calculated
       calc7ma = ifelse(startdate7 == (Date - 6), 1, 0 ),
       dosat_mean7= ifelse(calc7ma == 1, round(rollmean(x = DO_sat, 7, align = "right", fill = NA),1) , NA )) 


# Join DOsat to 7_D metrics -----------------------------------------------
DO_sat_join <- DO_sat_7dma %>%
  select(MLocID, dosat_mean7, Date) 


spawn_DO_data <- continuous_data %>%
  mutate(Do_7D = Result4IR,
         Date = as.Date(ActStartD)) %>%
  left_join(DO_sat_join, by = c('MLocID', 'Date'))



# Cont spawn analysis -----------------------------------------------------


cont_spawn_Do_analysis <- spawn_DO_data %>%
  mutate(Violation = ifelse(Do_7D < 11.0 &
                              dosat_mean7 < 95.0, 1, 0 ))

write.csv(cont_spawn_Do_analysis, file = "Parameters/DO/Data Review/Spawning_Continuous_data_analysis.csv", row.names = FALSE)

cont_spawn_DO_categories <- cont_spawn_Do_analysis %>%
  group_by(AU_ID) %>%
  summarise(num_valid_samples = sum(!is.na(Violation)),
            num_violations = sum(Violation, na.rm = TRUE),
            category = ifelse(num_violations >= 2, "Cat 5", "Cat 2" )) %>%
  mutate(type = "Spawning continuous")

#write table here



# Instantaneous -----------------------------------------------------------

instantaneous_data <- Results_spawndates %>%
  filter(!AU_ID %in% unique(continuous_data_AUs$AU_ID),
         ResultBasesName == "Minimum" |
           is.na(ResultBasesName) )


instant_mon_locs <- unique(instantaneous_data$MLocID)



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


# Pare down temp table to be used for joining
instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
  select(MLocID, ResultBasesName, Result4IR, ActStartD, ActStartT, ActDepth) %>%
  rename(Temp_res = Result4IR)


# Join DO and temp tables and calculate DO-Sat
instant_DO_sat <- instant_perc_sat_DO %>%
  rename(DO_res =  Result4IR) %>%
  left_join(instant_perc_sat_temp_join, by = c('MLocID', 'ActStartD', 'ActStartT', 'ResultBasesName', 'ActDepth')) %>%
  mutate(DO_sat = DOSat_calc(DO_res, Temp_res, ELEV_Ft )) 




instant_DO_sat_analysis <- instant_DO_sat %>%
  mutate(Violation = ifelse(DO_res < 11.0 & DO_sat < 95.0, 1, 0 ))

write.csv(instant_DO_sat_analysis, file = "Parameters/DO/Data Review/Spawning_instantaneous_data_analysis.csv", row.names = FALSE)

instant_DO_sat_categories <- instant_DO_sat_analysis %>%
  group_by(AU_ID) %>%
  summarise(num_samples = n(),
            num_Violations = sum(Violation, na.rm = TRUE)) %>%
  mutate(critical_excursions = excursions_conv(num_samples)) %>%
  mutate(category = ifelse(num_samples < 10 &
                             num_Violations == 0, "Cat 3", 
                           ifelse(num_samples < 10 &
                                    num_Violations > 0, "Cat 3B", 
                                  ifelse(num_samples > 10 & num_Violations > critical_excursions, "Cat 5", 
                                         ifelse(num_samples > 10 & num_Violations <= critical_excursions, "Cat 2", 
                                                "ERROR"))))) %>%
  mutate(type = "Spawning instant")

# Write table here


return(list(cont_spawn_DO_categories,instant_DO_sat_categories ))

}

