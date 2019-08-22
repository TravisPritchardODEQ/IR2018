library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)



# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")

# Pull selected data from InputRaw.
IR_Res_qry <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name in ('Dissolved oxygen saturation', 'Dissolved oxygen (DO)', 'pH') OR
(Char_Name ='Temperature, water'
        AND Statistical_Base = '7DADM')"

IR_res <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))

# Pull selected data from ResultsRawWater2018. Thi sis use dto get act_id
join_query <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height, act_id
  FROM [IntegratedReport].[dbo].[ResultsRawWater2018] 
  WHERE Char_Name in ('Dissolved oxygen saturation', 'Dissolved oxygen (DO)', 'pH') OR
(Char_Name ='Temperature, water'
        AND Statistical_Base = '7DADM')"


join_table  <- DBI::dbGetQuery(IR.sql, glue_sql(join_query, .con = IR.sql)) %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height ))

# Pull result_UIDs from the data views. This allows filtering the data to only what is being used for the IR
View_results <- "SELECT [Result_UID], Char_Name FROM [IntegratedReport].[dbo].[VW_DO]
UNION 
SELECT [Result_UID], Char_Name FROM [IntegratedReport].[dbo].[VW_Temperature]
UNION 
SELECT [Result_UID], Char_Name FROM [IntegratedReport].[dbo].[VW_pH]"

view_results_table <- DBI::dbGetQuery(IR.sql, glue_sql(View_results, .con = IR.sql))

# Group data by date and activity type
# Calculate nu,ber of samples in group
# Calculate number of unique results in group - this allows for checking for duplcaites
# Filter down to groups with > 1 result indicated multiple samples on same day at teh same location
# Add a variable for group which ties the groupings in together
# join to the join  table to get act_ids - this helps with backtracing the data.
grouped_no_res_no_time <- IR_res %>%
  filter(Result_UID %in% view_results_table$Result_UID) %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(OrganizationID, 
           MLocID,
           Char_Name,
           #Activity_Type, 
           SampleStartDate,
           #SampleStartTime,
           Char_Name, 
           Statistical_Base,
           #IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n(),
            num_distinct_results = n_distinct(IRResultNWQSunit)) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(Char_Name, OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table, by = c('OrganizationID',
                               'MLocID',
                               'Char_Name',
                               #'Activity_Type',
                               'SampleStartDate',
                               'Char_Name',
                               'Statistical_Base',
                               'act_depth_height')) %>%
  filter(Result_UID %in% view_results_table$Result_UID)

######################
NALMS_data_to_remove <- grouped_no_res_no_time %>%
  filter(OrganizationID == "NALMS" & Char_Name == 'Dissolved oxygen (DO)') %>%
  mutate(code = "44",
         rational = "Suspect data. All results are integers") %>%
  select(Result_UID, Char_Name, code, rational)

grouped_no_res_no_time <- grouped_no_res_no_time %>%
  filter(!Result_UID %in% NALMS_data_to_remove$Result_UID)


# get all data from input raw for joining. This isn't really needed right now, but jelps figure out what is going on with the duplicates
IR_Res_qry <- "Select  *
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name in ('Dissolved oxygen saturation', 'Dissolved oxygen (DO)', 'pH') OR
(Char_Name ='Temperature, water'
        AND Statistical_Base = '7DADM')"

all_data  <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql)) %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height ))
  





dups <- grouped_no_res_no_time %>%
  select(Result_UID,
         num,
         num_distinct_results,
         group,
         act_id) %>%
  left_join(all_data, by = "Result_UID" ) %>%
  filter(num_distinct_results == 1) %>%
  group_by(group) %>%
  mutate(code = ifelse(row_number() > 1, "46", ""),
         rational = ifelse(code == 46, "Suspected Duplicate", "" )
  )


depth <- grouped_no_res_no_time %>%
  select(Result_UID,
         num,
         num_distinct_results,
         group,
         act_id) %>%
  left_join(all_data, by = "Result_UID" ) %>%
  filter(num_distinct_results > 1) %>%
  group_by(group) %>%
  mutate(is_highest_QL = ifelse(QualifierAbbr == min(QualifierAbbr), 'Yes', 'No'),
         num_QL_levels = n_distinct(is_highest_QL, na.rm = FALSE),
         drop_DQL = ifelse(num_QL_levels > 1 & is_highest_QL == 'No', 'Yes', "No"),
         code = ifelse(drop_DQL == 'Yes', "52", "" ),
         rational = ifelse(drop_DQL == "Yes", "Multiple results at datetime and higher DQL exists", ""),
         num_depths = n_distinct(act_depth_height)
  ) 

# manual Review
write.csv(filter(depth,code ==""), "Other tools/dup_check_manual_review.csv")

manual_review <- read.csv("Other tools/dup_check_manual_review.csv", stringsAsFactors = FALSE)



manual_removed <- manual_review %>%
  filter(!is.na(code) & code != "52") %>%
  select(Result_UID, Char_Name, code, rational) %>%
  mutate(Result_UID = as.numeric(Result_UID),
         code = as.character(code))

depth <- depth %>%
  filter(!Result_UID %in% manual_removed$Result_UID)

######################################################################################
threshold_calculation <- depth %>%
  ungroup() %>%
  filter(code == "") %>%
  group_by(group) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  mutate(diff = max(IRResultNWQSunit) - min(IRResultNWQSunit)) %>%
  mutate(above_threshold = case_when(Char_Name == 'Temperature, water' & diff > 1.0 ~ 1,
                               Char_Name == 'pH' & diff > 0.5 ~ 1,
                               Char_Name == 'Dissolved oxygen (DO)' & diff > 1.0 ~ 1,
                               TRUE ~ 0))

#####################################################################
# Add these to IR database for exclusion ----------------------------------

low_QL <- depth %>%
  ungroup() %>%
  filter(code != "") %>%
  select(Result_UID, Char_Name, code, rational)

dups_to_remove <- dups %>%
  ungroup() %>%
  filter(code != "") %>%
  select(Result_UID, Char_Name, code, rational)

out_of_theshold <- threshold_calculation %>%
  ungroup() %>%
  filter(above_threshold == 1) %>%
  mutate(code = "44",
         rational = "Duplicate data outside of DQL=B quality threshold") %>%
  select(Result_UID, Char_Name, code, rational)

add_to_UnusedData_2018 <- low_QL %>%
  bind_rows(dups_to_remove) %>%
  bind_rows(out_of_theshold) %>%
  bind_rows(NALMS_data_to_remove) %>%
  bind_rows(manual_removed)

########################################################################

# Data to aggregate -------------------------------------------------------
# if pH - convert pH to H concentration, average those, and then recalulate into pH
aggregate_data <- threshold_calculation %>%
  filter(above_threshold == 0) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(H_concentration = ifelse(Char_Name == 'pH', 10^(-IRResultNWQSunit), NA )) %>% 
  mutate(mean_result = case_when(Char_Name == 'pH' ~ round(-log10(mean(H_concentration)), 2),
                                 Char_Name ==  'Dissolved oxygen (DO)' ~ min(IRResultNWQSunit),
                                 TRUE ~ mean(IRResultNWQSunit))) %>%
  select(Result_UID, group, mean_result)

save(aggregate_data, file = "Other tools/aggregate_data.Rdata")


# Writedata to IR database for exclusion ----------------------------------

#write csv file of data to be excluded
write.csv(add_to_UnusedData_2018, "Other tools/excluded_datav2.csv")

add_to_db <- add_to_UnusedData_2018 %>%
  rename(Data_Review_Code = code,
         Datat_Review_Comment = rational)


DBI::dbWriteTable(IR.sql, 'UnusedData_2018', value= add_to_db, append = TRUE)
