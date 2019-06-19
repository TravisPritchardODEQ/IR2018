
DO_estuary_spawn <- function(df){
  
  
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
           SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - years(1), # subtract a year if in spawn period carrying from previous year
                                SpawnStart),
           in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 )) %>%
    filter(in_spawn == 1, !is.na(AU_ID)) %>%
    filter(!is.null(OWRD_Basin) & DO_code %in% c(1))
  
  
  
  # Continuous --------------------------------------------------------------
  
  
  #  get counts of number of results per ResultBasesName and AU_ID
  Results_spawndates_counts <- Results_spawndates %>%
    group_by(AU_ID, Statistical_Base) %>%
    summarise(count = n())
  
  
  
  # Instantaneous -----------------------------------------------------------
  
  
  # Get table of data from orginial to be analyzed using instantaneous metrics
  # Filters on AU's not found in continuous_data_AUs$AU_ID
  # filter down to only daily minimums or
  # ResultBasesName thatare null, indicated grab samples
  

  instantaneous_data <- Results_spawndates %>%
    filter(Statistical_Base == "Minimum" |
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
  
  # Remove duplicate DO sat Values that are mistakenly in AWQMS
  instant_perc_sat_DO_AWQMS <- instant_perc_sat_DO_AWQMS %>%
    distinct(MLocID, SampleStartDate,SampleStartTime,Statistical_Base, .keep_all = TRUE)
  
  
  instant_perc_sat_DO <- instant_perc_sat_DO %>%
    left_join(instant_perc_sat_DO_AWQMS, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))
  
  
  
  # Pare down temp table to be used for joining
  instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
    select(MLocID, Statistical_Base, IRResultNWQSunit, SampleStartDate, SampleStartTime, act_depth_height) %>%
    rename(Temp_res = IRResultNWQSunit)
  
  #aggregate duplicate data
  load("Other tools/aggregate_data.Rdata")
  data_to_agg <- instant_perc_sat_DO %>%
    left_join(aggregate_data, by = 'Result_UID') %>%
    filter(!is.na(group)) %>%
    arrange(group) %>%
    group_by(group) %>%
    mutate(mean_sat = mean(DO_sat, na.rm = TRUE),
           analysis_comment = paste0(Result_UID, collapse = ", "),
           keep = ifelse(row_number() == 1, 1, 0 )) %>%
    mutate(mean_sat = ifelse(is.nan(mean_sat), NA, mean_sat)) %>%
    mutate(analysis_comment = paste("Result is the average of result_UIDs:",analysis_comment, " - due to multiple results at same date")) %>%
    ungroup() %>%
    select(Result_UID, mean_result, mean_sat, keep, analysis_comment)
  
  results_analysis <- instant_perc_sat_DO %>%
    left_join(data_to_agg, by = "Result_UID") %>%
    filter(keep == 1 | is.na(keep)) %>%
    mutate(IRResultNWQSunit = ifelse(!is.na(mean_result), mean_result, IRResultNWQSunit ),
           DO_sat = ifelse(!is.na(mean_sat), mean_sat, DO_sat )) %>%
    select(-mean_result, -keep, -mean_sat)
  
  # Join DO and temp tables and calculate DO-Sat
  instant_DO_sat <- results_analysis %>%
    mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
           SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
           SpawnStart = mdy(SpawnStart),
           SpawnEnd = mdy(SpawnEnd),
           SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
           in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 )) %>%
    filter(in_spawn == 1, !is.na(AU_ID)) %>%
    filter(!is.null(OWRD_Basin) & DO_code %in% c(1)) %>%
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
    rename(IRResultNWQSunit = DO_res)
  
  
  
  export <- DO_Dup_remover(export, filename = "Parameters/DO/DO_Spawn_instant_Duplicated.csv")
  IR_export(export, "Parameters/DO/Data_Review", "DO_estuary_instant_Spawn", "data" )
  
  
  
  
  #write.csv(instant_DO_sat_analysis, file = "Parameters/DO/Data Review/Spawning_instantaneous_data_analysis.csv", row.names = FALSE)
  
  instant_DO_sat_categories <- export %>%
    group_by(AU_ID) %>%
    summarise(OWRD_Basin = first(OWRD_Basin), 
              num_samples = n(),
              num_Violations = sum(Violation, na.rm = TRUE)) %>%
    mutate(critical_excursions = excursions_conv(num_samples)) %>%
    mutate(IR_category = case_when(num_samples < 5 & num_Violations == 0 ~ "Cat 3",
                                   num_samples < 5 & num_Violations > 0 ~ "Cat 3B",
                                   num_samples >= 5 & num_Violations >= critical_excursions ~  "Cat 5",
                                   num_samples >= 5 & num_Violations < critical_excursions ~ "Cat 2",
                                   TRUE ~ 'ERROR')) %>%
    mutate(type = "Spawning instant")
  
  # Write table here
  
  
  return(instant_DO_sat_categories )
  
  #IR_export(cont_spawn_DO_categories, "Parameters/DO/Data_Review", "DO_estuary_continuous_Spawn", "categories")
  IR_export(instant_DO_sat_categories, "Parameters/DO/Data_Review", "DO_estuary_instant_Spawn", "categories")
  
}

