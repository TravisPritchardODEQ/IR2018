library(tidyverse)


# DO depth removal --------------------------------------------------------
database <- "IR 2018"
print("Fetch DO data from IR database")
#connect to IR database view as a general user
# import Temperature data
IR.sql <-   odbcConnect(database)


# Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
# Join with Crit_Temp to get temperature Criteria and spawn ?
Results_import <-    sqlFetch(IR.sql, "dbo.VW_DO") 


odbcClose(IR.sql)



print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations in",
            length(unique(Results_import$AU_ID)), "AUs"))

# Set factors to characters
Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import


# Data validation ---------------------------------------------------------

Results_import <- Results_import %>%
  filter(is.na(Statistical_Base) | Statistical_Base != 'Delta')


# Remove depth data -------------------------------------------------------

print("removing data at depth")  
not_closest_to_surface<- Results_import %>%
  filter(!is.na(act_depth_height) & act_depth_height <= 1) %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           Char_Name, 
           Statistical_Base
           #IRResultNWQSunit,
           #act_depth_height
  ) %>%
  mutate(is_min = ifelse(act_depth_height == min(act_depth_height), 1, 0 )) %>%
  filter(is_min == 0)



data_to_remove <- Results_import %>%
  mutate(remove = case_when(Result_UID %in% not_closest_to_surface$Result_UID ~ 1, 
                            General_Comments == "Bottom" ~ 1,
                            act_depth_height > 1 & ActDepthUnit == 'm' |
                              act_depth_height > 3.28 & ActDepthUnit == 'ft' ~ 1,
                            TRUE ~ 0),
         remove_comments = case_when(Result_UID %in% not_closest_to_surface$Result_UID ~ "Result Closer to surface exists on same day", 
                                     General_Comments == "Bottom" ~ "Result labeled as 'Bottom' on comments",
                                     act_depth_height > 1 & ActDepthUnit == 'm' |
                                       act_depth_height > 3.28 & ActDepthUnit == 'ft' ~ "activity depth > 1 m",
                                     TRUE ~ "")) %>%
  filter(remove == 1)

print("Writing DO_excluded_depth_data.csv") 

write.csv(data_to_remove, "Parameters/DO/Data_Review/DO_excluded_depth_data.csv", row.names = FALSE)  


odbcClose(IR.sql)


DO_data_to_exclude <- data_to_remove %>%
  mutate(Data_Review_code = '53') %>%
  select(Result_UID, Char_Name, Data_Review_code, remove_comments) %>%
  rename(Datat_Review_Comment = remove_comments)

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

DBI::dbWriteTable(con, 'UnusedData_2018', value= DO_data_to_exclude, append = TRUE)



# pH ----------------------------------------------------------------------

print("Fetch pH data from IR database")
#connect to IR database view as a general user
# import bacteria data
IR.sql <-   odbcConnect(database)


# Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
# Join with Crit_Temp to get temperature Criteria and spawn ?
Results_import <-    sqlFetch(IR.sql, "dbo.VW_pH") 


odbcClose(IR.sql)

print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), 
            "monitoring locations in", length(unique(Results_import$AU_ID)), "AUs"))

# Set factors to characters
Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import

# Censored data -----------------------------------------------------------

print("Modify censored data")

#run the censored data function to set censored data. This will use the lowest crit value from above
Results_censored <- Censored_data(Results_import, crit = `pH_Min` ) %>%
  mutate(Result_cen = as.numeric(Result_cen))

print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))

Results_censored <- Results_censored %>%
  filter(!is.na(Result_cen))

not_closest_to_surface<- Results_censored %>%
  filter(!is.na(act_depth_height) & act_depth_height <= 1) %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           Char_Name, 
           Statistical_Base
           #IRResultNWQSunit,
           #act_depth_height
  ) %>%
  mutate(is_min = ifelse(act_depth_height == min(act_depth_height), 1, 0 )) %>%
  filter(is_min == 0)



data_to_remove <- Results_import %>%
  mutate(remove = case_when(Result_UID %in% not_closest_to_surface$Result_UID ~ 1, 
                            General_Comments == "Bottom" ~ 1,
                            act_depth_height > 1 & ActDepthUnit == 'm' |
                              act_depth_height > 3.28 & ActDepthUnit == 'ft' ~ 1,
                            TRUE ~ 0),
         remove_comments = case_when(Result_UID %in% not_closest_to_surface$Result_UID ~ "Result Closer to surface exists on same day", 
                                     General_Comments == "Bottom" ~ "Result labeled as 'Bottom' on comments",
                                     act_depth_height > 1 & ActDepthUnit == 'm' |
                                       act_depth_height > 3.28 & ActDepthUnit == 'ft' ~ "activity depth > 1 m",
                                     TRUE ~ "")) %>%
  filter(remove == 1)



pH_data_to_exclude <- data_to_remove %>%
  mutate(Data_Review_code = '53') %>%
  select(Result_UID, Char_Name, Data_Review_code, remove_comments) %>%
  rename(Datat_Review_Comment = remove_comments)

print("Writing pH_excluded_depth_data.csv") 

write.csv(data_to_remove, "Parameters/pH/Data_Review/pH_excluded_depth_data.csv", row.names = FALSE)  

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

DBI::dbWriteTable(con, 'UnusedData_2018', value= pH_data_to_exclude, append = TRUE)

