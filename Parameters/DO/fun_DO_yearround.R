
DO_year_round_analysis <- function(df){
  
  library(lubridate)
  library(odbc)
  library(glue)
  library(DBI)
  library(zoo)
  library(IRlibrary)
  

print("Beginning year round analysis")

print("Beginning continuous analysis")

# Year round --------------------------------------------------------------


# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod
Results_spawndates <- df %>%
  mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         critstart = mdy(paste0("7/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ))

# Summarize available data to get a list of AU's to be analyzed using cont. data
results_cont_summary <- Results_spawndates %>%
  filter(Statistical_Base == "30DADMean") %>%
  group_by(AU_ID) %>%
  summarise(tot_30d_metrics = n(),
            crit_30d_periods = sum(is.crit)) %>%
  filter(crit_30d_periods >= 15,
         !is.na(AU_ID))



# Initial Continuous criteria analysis --------------------------------------------

# This initial analysis is used to see where we need to calculate DO Sat 
# Calculating the 30DADMean DO SAt is computationally expensive
# so we only calculate it at locations where it woudl influnce the
# IR category

# filter down to AUs that are to be evaluated with cont metrics
# Filter down to only 30-D, 7-Mi, and daily minimums
# Flag various violations
continuous_data_analysis <- Results_spawndates %>%
  filter(AU_ID %in% results_cont_summary$AU_ID) %>%
  filter(Statistical_Base %in% c("30DADMean", "7DADMin", "Minimum")) %>%
  mutate(Violation = ifelse(Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D, 1, 
                            ifelse(Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi, 1, 
                                   ifelse(Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min, 1, 0 )))) 


# Run through initial categorization
# This all gets redone in the end
# Where percent saturation would make a difference, set category as "Check percent Sat"
continuous_data_categories <- continuous_data_analysis %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(Total_violations = sum(Violation),
            Sum_30D_violations = sum(Violation [Statistical_Base == "30DADMean"]),
            Sum_7mi_violations = sum(Violation [Statistical_Base == "7DADMin"]),
            Sum_abs_min_violations = sum(Violation [Statistical_Base == "Minimum"])) %>%
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


# Datatable of results that need percent saturation
cont_perc_sat_check <- continuous_data_analysis %>%
  filter(AU_ID %in% unique(subset(continuous_data_categories, category == "Check percent Sat" )$AU_ID) )

# List of monitoring locations that need OD sat 
# This list is used for the sql query that follows
continuous_mon_locs <- unique(cont_perc_sat_check$MLocID)


# Get data from database --------------------------------------------------

print("querying the IR database to get data for DO sat calculations ")

# Get DO IR_database to calculate percent sat --------

Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

Doqry <- glue::glue_sql(Doqry, .con = con)

perc_sat_DO <- DBI::dbGetQuery(con, Doqry)

#Get temperature data from database

tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"

tempqry <- glue::glue_sql(tempqry, .con = con)

perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)

# Disconnect from database
DBI::dbDisconnect(con)

print("Finished database query")


# Join --------------------------------------------------------------------

# Pare down table to be used in join
perc_sat_temp_join <- perc_sat_temp %>%
  select(MLocID, IRResultNWQSunit, SampleStartDate, SampleStartTime, Statistical_Base) %>%
  rename(Temp_res = IRResultNWQSunit)

# Rename the result to DO_res and join with the temperature
# Calculate DOsat
DO_sat <- perc_sat_DO %>%
  rename(DO_res =  IRResultNWQSunit) %>%
  left_join(perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base')) %>%
  mutate(DO_sat = DOSat_calc(DO_res, Temp_res, ELEV_Ft ),
         ma.DOS.mean30 = "")

# calculate 30-D averages



#Create list that will be used to get data out of the loop
monloc_do_list <- list()

#Set loop for each monitoring location
print("Beginning DO sat Calculations")

for(i in 1:length(unique(DO_sat$MLocID))){
  
  print(paste("Station", i, "of", length(unique(DO_sat$MLocID))))
  
  #Name of station be be used in this loop iteration
  station = unique(DO_sat$MLocID)[i]
  
  #Filter dataset to only look at 1 monitoring location at a time
  daydat_station <- DO_sat %>%
    filter(MLocID == station) %>%
    mutate(startdate30 = as.Date(SampleStartDate) -30) %>%
    arrange(SampleStartDate)
  
 # Begin 30-d moving averages loop
   print("Begin 30 day moving averages" )
  pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
  
  for(l in 1:nrow(daydat_station)){
    
    #Beginning of 30 day window
    start30 <- daydat_station$startdate30[l]
    # End of 30 day window
    end30 <- daydat_station$SampleStartDate[l] 
    
    
    # For each row in table, crate a new datatable for taht row plus all
    # Results that are in the 30 day window
    station_30day <- daydat_station %>%
      filter(SampleStartDate <= end30 & SampleStartDate >= start30) 
    
    
    # If there are at least 29 values in the 30 day window
    # Calculate the average DO-Sat
    # Otherwise use NA
    ma.mean30 <- ifelse(length(unique(station_30day$SampleStartDate)) >= 29, mean(station_30day$DO_sat), NA )
    
    
    # Pass the 30-d DO Sat vaule back into the single monitoring location table
    # the l >+ 30 prevents the 29th day being used. 
    daydat_station[l,"ma.DOS.mean30"] <- ifelse(l >= 30, round(ma.mean30, 2), NA)
    setTxtProgressBar(pb, l)
  } #end of 30day loop
  
  # Assign dataset filtered to 1 monitoring location to a list for combining outside of for loop
  monloc_do_list[[i]] <- daydat_station
  
}

print("Finished DO Sat Calculations")
  
# Bind rows to get DO_sat averages

DO_sat_avgs <-  bind_rows(monloc_do_list)  


# Join DOsat to 30_D metrics -----------------------------------------------

# Add Statistical_Base to the DO Sat table
# Create Date field to be used for the join
# The Activity start dates were slighly different causing problems
# (1/1/1900 vs 1/1/1900 00:00)
DO_sat_join <- DO_sat_avgs %>%
  mutate(Statistical_Base = "30DADMean",
         Date = as.Date(SampleStartDate)) %>%
  select(MLocID, ma.DOS.mean30, Date,Statistical_Base) 


# Join DO Sat back into the original data table and recalculate violations
yr_round_cont_DO_data_analysis <- continuous_data_analysis %>%
  mutate(Date = as.Date(SampleStartDate)) %>%
  left_join(DO_sat_join, by = c('MLocID', 'Date', 'Statistical_Base')) %>%
  mutate(Violation = ifelse(DO_Class == "Cold Water"& 
                              Statistical_Base == "30DADMean" & 
                              IRResultNWQSunit < crit_30D & 
                              (ma.DOS.mean30 < 90 | is.na(ma.DOS.mean30)), 1,
                            ifelse(DO_Class != "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D, 1, 
                              ifelse(Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi, 1, 
                                   ifelse(Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min, 1, 0 ))))) 


# Write this table to a file to be used for the data review  
write.csv(yr_round_cont_DO_data_analysis, file = "Parameters/DO/Data Review/yearround_continuous_data_analysis.csv", row.names = FALSE)


# Summarise data and 
# Set the categories basedon flow charts
yr_round_cont_data_categories <- continuous_data_analysis %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(Total_violations = sum(Violation),
            Sum_30D_violations = sum(Violation [Statistical_Base == "30DADMean"]),
            Sum_7mi_violations = sum(Violation [Statistical_Base == "7DADMin"]),
            Sum_abs_min_violations = sum(Violation [Statistical_Base == "Minimum"])) %>%
  mutate(category = ifelse(Sum_30D_violations >= 2 |
                                Sum_7mi_violations >= 2 |
                                Sum_abs_min_violations >= 2, "Cat 5", 
                             ifelse(Sum_30D_violations < 2 &
                                                  Sum_7mi_violations < 2 &
                                                  Sum_abs_min_violations < 2, "Cat 2", "Error" )))




# Insantaneous metrics ----------------------------------------------------

# Analyze year round criteria using instantaneous metrics
print("Beginning instantaneous analysis")


# Begin preliminary analysis

# Create data table of data needed to use instant metrics
# Au's not found in results_cont_summary$AU_ID
# ResultBases Name of Minimum (to use cont data as instant)
# and NA which indicates grab data. 
# set preliminary violations based on results < 30D criteria
instant_data_analysis <- Results_spawndates %>%
  filter(!AU_ID %in% results_cont_summary$AU_ID) %>%
  filter(Statistical_Base %in% c("Minimum", NA)) %>%
  mutate(Violation_crit = ifelse(IRResultNWQSunit < crit_30D, 1, 0 ))


# assign initial categories
# Where percent saturation would make a difference, set category as "Check percent Sat"
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

# vector of monitoring locations to check DO saturdation. Used for database query
instant_mon_locs <- unique(inst_perc_sat_check$MLocID)
  

# Get data from database --------------------------------------------------


print("querying the IR database to get data for DO sat calculations ")

# Get DO and temp data from IR_database to calculate percent sat --------

# query DO data using instant_mon_locs as a monitoring location filter
Doqry <- "SELECT * 
FROM            VW_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

Doqry <- glue::glue_sql(Doqry, .con = con)

instant_perc_sat_DO <- DBI::dbGetQuery(con, Doqry)

# query temp data using instant_mon_locs as a monitoring location filter

tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"

tempqry <- glue::glue_sql(tempqry, .con = con)

instant_perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)

DBI::dbDisconnect(con)

print("Finished database query")

#Join toables together

# Pare down temp table to be used for joining
instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
  select(MLocID, Statistical_Base, IRResultNWQSunit, SampleStartDate, SampleStartTime, act_depth_height
) %>%
  rename(Temp_res = IRResultNWQSunit)


# Join DO and temp tables and calculate DO-Sat
instant_DO_sat <- instant_perc_sat_DO %>%
  rename(DO_res =  IRResultNWQSunit) %>%
  left_join(instant_perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base', 'act_depth_height
')) %>%
  mutate(DO_sat = DOSat_calc(DO_res, Temp_res, ELEV_Ft ))  %>%
  select(MLocID, SampleStartDate, SampleStartTime, Statistical_Base, act_depth_height
,DO_sat ) %>%
  mutate(ActDepth = as.numeric(act_depth_height
))


#Join back in and recalculate violations
# if do sat could not be calculated, then violation if IRResultNWQSunit < 30D criteria
Instant_data_analysis_DOS <- Results_spawndates %>%
  filter(!AU_ID %in% results_cont_summary$AU_ID) %>%
  filter(Statistical_Base %in% c("Minimum", NA)) %>%
  left_join(instant_DO_sat, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base', 'act_depth_height')) %>%
  mutate(Violation = ifelse(DO_Class == "Cold Water" & 
                              IRResultNWQSunit < crit_30D & 
                              (DO_sat < 90.0 | is.na(DO_sat) ), 1, 
                            ifelse(DO_Class != "Cold Water" & 
                                     IRResultNWQSunit < crit_30D, 1, 0))  )

# Write table to be used for data review
write.csv(Instant_data_analysis_DOS, file = "Parameters/DO/Data Review/yearround_instantaneous_data_analysis.csv", row.names = FALSE)


# Reassign categories based on flow charts
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

print("Year round analysis finished")


# Since functions cannot return two items, we stick the 
# Continuous and instant data tables into a list
# and we will seperate them outside of the function
return(list(yr_round_cont_data_categories,yr_round_instant_categories ))

}

  

# TO do

# Bring in DO sat directly, only join calculated DOsats where DOsat is null after join with data query