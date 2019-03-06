
DO_spawning_analysis <- function(df){


library(lubridate)
library(odbc)
library(glue)
library(DBI)
library(zoo)
library(IRlibrary)


# Spawning --------------------------------------------------------------






# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod
Results_spawndates <- df %>%
  mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 )) %>%
  filter(in_spawn == 1, !is.na(AU_ID)) %>%
  filter(!is.null(OWRD_Basin) & DO_code %in% c(2,3,4))




# Continuous --------------------------------------------------------------


#  get counts of number of results per ResultBasesName and AU_ID
Results_spawndates_counts <- Results_spawndates %>%
  group_by(AU_ID, Statistical_Base) %>%
  summarise(count = n())


# Data table of results of 7DADMean with more than 15 values
continuous_data_AUs <- Results_spawndates_counts %>%
  filter(Statistical_Base == "7DADMean",
         count >= 15)


# This table is the table of data that will be used for evaulation
continuous_data <- Results_spawndates %>%
  filter(AU_ID %in% unique(continuous_data_AUs$AU_ID),
         Statistical_Base == "7DADMean" )


# The monitoring locations that have data that meets continuous metrics criteria
continuous_mon_locs <- unique(continuous_data$MLocID)


# Get DO and temp data from IR_database to calculate percent sat --------


con <- DBI::dbConnect(odbc::odbc(), "IR 2018")



#Query DOSat from AWQMS
DOsat_AWQMS <- "SELECT [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
WHERE   Char_Name = 'Dissolved oxygen saturation' AND 
        MLocID in ({continuous_mon_locs*}) AND 
        Statistical_Base = 'Mean'"


DoSatqry <- glue::glue_sql(DOsat_AWQMS, .con = con)

perc_sat_AWQMS_DOSat <- DBI::dbGetQuery(con, DoSatqry)



# Query out the daily mean DO values from the indentified monitoring locations
Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"


Doqry <- glue::glue_sql(Doqry, .con = con)

perc_sat_DO <- DBI::dbGetQuery(con, Doqry)


# Query out the mean temp values from the indentified monitoring locations
tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"

tempqry <- glue::glue_sql(tempqry, .con = con)

perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)

DBI::dbDisconnect(con)


# Modfy DOSat table from AWQMS and push to perc_sat_DO --------------------------------------------------

# This ensures that if AWQMS has DOSat values, we use those, and only calculate DOsat values
# Where we don't already have them


perc_sat_DO <- perc_sat_DO %>%
  left_join(perc_sat_AWQMS_DOSat, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))



# Join Do and temp and calculate DOSat ------------------------------------

# Pare down the temperature table to be used to join
perc_sat_temp_join <- perc_sat_temp %>%
  select(OrganizationID,MLocID, IRResultNWQSunit, SampleStartDate, SampleStartTime) %>%
  rename(Temp_res = IRResultNWQSunit)


# prep the imported DO table to be joined
# Rename the DO result to DO_res
# Join the DO table to the pared down temperature table
# Calculate DOSat
DO_sat <- perc_sat_DO %>%
  rename(DO_res =  IRResultNWQSunit) %>%
  left_join(perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime')) %>%
  mutate(DO_sat = ifelse(is.na(DO_sat), DOSat_calc(DO_res, Temp_res, ELEV_Ft ), DO_sat ),
         DO_sat = ifelse(DO_sat > 100, 100, DO_sat )) 


# Calculate moving 7 day average
# create flag for which results have 7 days worth of data
# calculate 7 day moving average of DO_Sat off of daily mean DO_Sat
DO_sat_7dma <- DO_sat %>%
  mutate(Date = as.Date(SampleStartDate)) %>%
  arrange(MLocID, SampleStartDate) %>%
  group_by(MLocID) %>%
  mutate(startdate7 = lag(Date, 6, order_by = Date),
       # flag out which result gets a moving average calculated
       calc7ma = ifelse(startdate7 == (Date - 6), 1, 0 ),
       dosat_mean7= ifelse(calc7ma == 1, round(rollmean(x = DO_sat, 7, align = "right", fill = NA),1) , NA )) 


# Join DOsat to 7_D metrics -----------------------------------------------

# Pare down the table of DO_Sats so it can be joined
DO_sat_join <- DO_sat_7dma %>%
  select(MLocID, dosat_mean7, Date) 



# Join DO_Sat values to the table that will be used for evaluation
spawn_DO_data <- continuous_data %>%
  mutate(Do_7D = IRResultNWQSunit,
         Date = as.Date(SampleStartDate)) %>%
  left_join(DO_sat_join, by = c('MLocID', 'Date'))



# Cont spawn analysis -----------------------------------------------------



# Mark result as violation if DO_7D < 11 and the sdosat_7day is < 95
cont_spawn_Do_analysis <- spawn_DO_data %>%
  mutate(Violation = ifelse((Do_7D < 11.0 &
                              dosat_mean7 < 95.0) |
                              (Do_7D < 11.0 &
                                 is.na(dosat_mean7)), 1, 0 ))


print("Writing continuous spawning data tables")

export <- cont_spawn_Do_analysis %>%
  select(-Do_7D)

IR_export(export, "Parameters/DO/Data_Review", "DO_Continuous_Spawn", "data" )




# daily minimum counts ----------------------------------------------------

daily_minimums <- Results_spawndates %>%
  filter(Statistical_Base == "Minimum",
         MLocID %in% unique(cont_spawn_Do_analysis$MLocID)) %>%
  group_by(AU_ID) %>%
  summarise(num_below_abs_min = sum(IRResultNWQSunit < 9))



# Categorize based on the analysis. 
# this table gets returned in the function
# Summarize violations and number of samples
# If there are 2 of more violations - Cat 5, 
# Else Cat 2
cont_spawn_DO_categories <- cont_spawn_Do_analysis %>%
  group_by(AU_ID) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            #num_valid_samples = sum(!is.na(Violation)),
            num_violations = sum(Violation, na.rm = TRUE)) %>%
  left_join(daily_minimums, by = 'AU_ID') %>%
  mutate(category = ifelse(num_violations >= 2 | num_below_abs_min >= 2 , "Cat 5", 
                           "Cat 2" )) #%>%
 # mutate(type = "Spawning continuous")

#write table here



# Instantaneous -----------------------------------------------------------


# Get table of data from orginial to be analyzed using instantaneous metrics
# Filters on AU's not found in continuous_data_AUs$AU_ID
# filter down to only daily minimums or
# ResultBasesName thatare null, indicated grab samples
instantaneous_data <- Results_spawndates %>%
  filter(!AU_ID %in% unique(continuous_data_AUs$AU_ID),
         Statistical_Base == "Minimum" |
           is.na(Statistical_Base) )


# List of monitoring locations found in above data table, used to put together 
# data query from IR database
instant_mon_locs <- unique(instantaneous_data$MLocID)



# Get DO and temp data from IR_database to calculate percent sat --------

#Query Dsat

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")
#Query DOSat from AWQMS

DOsat_AWQMS <- "SELECT [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
WHERE ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*}) AND Char_Name = 'Dissolved oxygen saturation') OR 
      ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}) AND Char_Name = 'Dissolved oxygen saturation')"


Dosatqry <- glue::glue_sql(DOsat_AWQMS, .con = con)

instant_perc_sat_DO_AWQMS <- DBI::dbGetQuery(con, Dosatqry)

# Query DO data

Doqry <- "SELECT * 
FROM            VW_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"



Doqry <- glue::glue_sql(Doqry, .con = con)

instant_perc_sat_DO <- DBI::dbGetQuery(con, Doqry)


# Query temp data

tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"

tempqry <- glue::glue_sql(tempqry, .con = con)

instant_perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)

DBI::dbDisconnect(con)



instant_perc_sat_DO <- instant_perc_sat_DO %>%
  left_join(instant_perc_sat_DO_AWQMS, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))



# Pare down temp table to be used for joining
instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
  select(MLocID, Statistical_Base, IRResultNWQSunit, SampleStartDate, SampleStartTime, act_depth_height) %>%
  rename(Temp_res = IRResultNWQSunit)


# Join DO and temp tables and calculate DO-Sat
instant_DO_sat <- instant_perc_sat_DO %>%
  mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 )) %>%
  filter(in_spawn == 1, !is.na(AU_ID)) %>%
  filter(!is.null(OWRD_Basin) & DO_code %in% c(2,3,4)) %>%
  rename(DO_res =  IRResultNWQSunit) %>%
  left_join(instant_perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base', 'act_depth_height')) %>%
  mutate(DO_sat = ifelse(is.na(DO_sat),DOSat_calc(DO_res, Temp_res, ELEV_Ft ), DO_sat),
         DO_sat = ifelse(DO_sat > 100, 100, DO_sat )) 



# Violations are DO_res < 11 and DO_Sat < 95
instant_DO_sat_analysis <- instant_DO_sat %>%
  mutate(Violation = ifelse((DO_res < 11.0 & DO_sat < 95.0) |
                              (DO_res < 11.0 & is.na(DO_sat)) , 1, 0 ))



print("Writing instant spawning data tables")

export <- instant_DO_sat_analysis %>%
  select(-DO_res)

IR_export(export, "Parameters/DO/Data_Review", "DO_Instant_Spawn", "data" )




#write.csv(instant_DO_sat_analysis, file = "Parameters/DO/Data Review/Spawning_instantaneous_data_analysis.csv", row.names = FALSE)

instant_DO_sat_categories <- instant_DO_sat_analysis %>%
  group_by(AU_ID) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            num_samples = n(),
            num_Violations = sum(Violation, na.rm = TRUE)) %>%
  mutate(critical_excursions = excursions_conv(num_samples)) %>%
  mutate(category = ifelse(num_samples < 10 &
                             num_Violations == 0, "Cat 3", 
                           ifelse(num_samples < 10 &
                                    num_Violations > 0, "Cat 3B", 
                                  ifelse(num_samples > 10 & num_Violations >= critical_excursions, "Cat 5", 
                                         ifelse(num_samples > 10 & num_Violations < critical_excursions, "Cat 2",  
                                                "ERROR"))))) %>%
  mutate(type = "Spawning instant")

# Write table here


return(list(cont_spawn_DO_categories,instant_DO_sat_categories ))

}

