# 5/9/2019
# This script checks for potential duplicate values in the IR database that
# need to be addressed before assessment will work properly.
 
# Output is an excel spreadsheet that contains 6 tabs (2 for each parameter)
# The script is currently set up to check for DO, temperature, and ph results
# 

# For each parameter, the script will generate 2 tabs. One labeled as suspect
# duplicate and one labeled 'suspect rep, depth, x sec'.
# 
# The 'suspect duplicate' tab conatins results that have more than 1 result 
# that match the following columns:
#   -  OrganizationID, 
#   -  MLocID, 
#   -  Activity_Type, 
#   -  SampleStartDate,
#   -  SampleStartTime,
#   -  Char_Name, 
#   -  Statistical_Base,
#   -  IRResultNWQSunit,
#   -  act_depth_height
# 

# The group column creates a unique number that ties the suspected duplicates together and the
# num column indicate how many results are in that group
# Every result after the first one, revices a code of 46 and a rational of 
# Suspected Duplicate'. these Result_UIDs should be loaded into the UnusedData
# table in the IR database. 
# 
# The 'suspect rep, depth, x sec' table conatins results that have more than 1 result 
# that match the following columns:
#   -  OrganizationID, 
#   -  MLocID, 
#   -  Activity_Type, 
#   -  SampleStartDate,
#   -  SampleStartTime,
#   -  Char_Name, 
#   -  Statistical_Base,
#   -  act_depth_height
#   
# These values most likely represent unlabled depth profile, cross section,  replicates or
# splits. 
# 
# If a duplicate grouping contains a mix of DQL levels, the lower DQl levels are labled with 
# a code of '52' and a rational of Multiple results at datetime and higher DQL exists. 
# 
# The rest of the data needs to be manually checked.



library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)



IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")


IR_DO_Res_qry <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name in ('Dissolved oxygen saturation', 'Dissolved oxygen (DO)')"

IR_DO_res <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_Res_qry, .con = IR.sql))

join_query <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height, act_id
  FROM [IntegratedReport].[dbo].[ResultsRawWater2018] 
  WHERE Char_Name in ('Dissolved oxygen saturation', 'Dissolved oxygen (DO)')"


join_table  <- DBI::dbGetQuery(IR.sql, glue_sql(join_query, .con = IR.sql))

IR_DO_VW_query <- "Select Result_UID
  FROM [IntegratedReport].[dbo].[VW_DO]"

IR_DO_VW  <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_VW_query, .con = IR.sql))

DO_VW_Res_UID <- IR_DO_VW$Result_UID





DO_grouped <- IR_DO_res %>%
  filter(Result_UID %in% DO_VW_Res_UID) %>%
  filter(AU_ID != '99') %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Char_Name, 
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table)


DO_grouped_no_res <- IR_DO_res %>%
  filter(Result_UID %in% DO_VW_Res_UID) %>%
  filter(AU_ID != '99') %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Char_Name, 
           Statistical_Base,
           #IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table) %>%
  filter(Result_UID %in% IR_DO_res$Result_UID)

#write.csv(grouped, "DO_dup.csv")



Do_diff <- DO_grouped_no_res %>%
  filter(!Result_UID %in% DO_grouped$Result_UID)



######################



IR_DO_Res_qry <- "Select  *
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name in ('Dissolved oxygen saturation', 'Dissolved oxygen (DO)')"

DO_all_data  <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_Res_qry, .con = IR.sql))

DO_dups <- DO_all_data %>%
  right_join(select(DO_grouped, 
                    Result_UID,
                    num,
                    group,
                    act_id), by = "Result_UID") %>%
  group_by(group) %>%
  mutate(code = ifelse(row_number() > 1, 46, ""),
         rational = ifelse(code == 46, "Suspected Duplicate", "" )
         )

DO_depth <- DO_all_data %>%
  right_join(select(Do_diff, 
                    Result_UID,
                    num,
                    group,
                    act_id),
             by = "Result_UID") %>%
  group_by(group) %>%
  mutate(is_highest_QL = ifelse(QualifierAbbr == min(QualifierAbbr), 'Yes', 'No'),
         num_QL_levels = n_distinct(is_highest_QL, na.rm = FALSE),
         drop_DQL = ifelse(num_QL_levels > 1 & is_highest_QL == 'No', 'Yes', "No"),
         code = ifelse(drop_DQL == 'Yes', 52, "" ),
         rational = ifelse(drop_DQL == "Yes", "Multiple results at datetime and higher DQL exists", "")
  )




DBI::dbDisconnect(IR.sql)


# temperature -------------'Temperature, water'"------------------------------------------------

IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")


IR_DO_Res_qry <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name ='Temperature, water'
        AND Statistical_Base = '7DADM'"

IR_res_temp <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_Res_qry, .con = IR.sql))

join_query <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height, act_id
  FROM [IntegratedReport].[dbo].[ResultsRawWater2018] 
  WHERE Char_Name ='Temperature, water'
        AND Statistical_Base = '7DADM'"

join_table_temp  <- DBI::dbGetQuery(IR.sql, glue_sql(join_query, .con = IR.sql))

DBI::dbDisconnect(IR.sql)


Temp_grouped <- IR_res_temp %>%
  filter(AU_ID != '99') %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Char_Name, 
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table_temp)


Temp_grouped_no_res <- IR_res_temp %>%
  filter(AU_ID != '99') %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Char_Name, 
           Statistical_Base,
           #IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table_temp) %>%
  filter(Result_UID %in% IR_res_temp$Result_UID)

temp_diff <- Temp_grouped_no_res %>%
  anti_join(Temp_grouped, by = 'Result_UID')

# pH ----------------------------------------------------------------------



IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")


IR_DO_Res_qry <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name ='pH'"

IR_res_pH <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_Res_qry, .con = IR.sql))

join_query <- "Select  OrganizationID, [MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height, act_id
  FROM [IntegratedReport].[dbo].[ResultsRawWater2018] 
  WHERE Char_Name ='pH'"


join_table_pH  <- DBI::dbGetQuery(IR.sql, glue_sql(join_query, .con = IR.sql))

DBI::dbDisconnect(IR.sql)


pH_grouped <- IR_res_pH %>%
  filter(AU_ID != '99') %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Char_Name, 
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table_pH)


pH_grouped_no_res <- IR_res_pH %>%
  filter(AU_ID != '99') %>%
  group_by(OrganizationID, 
           MLocID, 
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Char_Name, 
           Statistical_Base,
           #IRResultNWQSunit,
           act_depth_height) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(OrganizationID, MLocID, SampleStartDate ) %>%
  mutate(group = row_number()) %>%
  left_join(join_table_pH) %>%
  filter(Result_UID %in% IR_res_pH$Result_UID)


pH_diff <- pH_grouped_no_res %>%
  anti_join(pH_grouped, by = 'Result_UID')










# get all data ---------------------------------------------------------------



IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")


IR_DO_Res_qry <- "Select  *
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name ='pH'"

pH_all_data  <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_Res_qry, .con = IR.sql))

ph_dups <- pH_all_data %>%
  right_join(select(pH_grouped, 
                    Result_UID,
                    num,
                    group,
                    act_id), by = "Result_UID")%>%
  group_by(group) %>%
  mutate(code = ifelse(row_number() > 1, 46, ""),
         rational = ifelse(code == 46, "Suspected Duplicate", "" ))


         pH_depth <- pH_all_data %>%
  right_join(select(pH_diff, 
                    Result_UID,
                    num,
                    group,
                    act_id),
             by = "Result_UID") %>%
  group_by(group) %>%
           mutate(is_highest_QL = ifelse(QualifierAbbr == min(QualifierAbbr), 'Yes', 'No'),
                  num_QL_levels = n_distinct(is_highest_QL, na.rm = FALSE),
                  drop_DQL = ifelse(num_QL_levels > 1 & is_highest_QL == 'No', 'Yes', "No"),
                  code = ifelse(drop_DQL == 'Yes', 52, "" ),
                  rational = ifelse(drop_DQL == "Yes", "Multiple results at datetime and higher DQL exists", "")
  )


non_QL_removed <- pH_depth %>%
  filter(num_QL_levels > 1 & drop_DQL == "No") %>%
  ungroup() %>%
  group_by(group)  %>%
  tally() %>%
  filter(n > 1)



pH_depth <- pH_depth %>%
  mutate(recheck = ifelse(drop_DQL == 'No' & group %in% non_QL_removed$group, "Yes", ""))




IR_DO_Res_qry <- "Select  *
  FROM [IntegratedReport].[dbo].[InputRaw] 
  WHERE Char_Name ='Temperature, water'
        AND Statistical_Base = '7DADM'"

temp_all_data  <- DBI::dbGetQuery(IR.sql, glue_sql(IR_DO_Res_qry, .con = IR.sql))

temp_dups <- temp_all_data %>%
  right_join(select(Temp_grouped, 
                    Result_UID,
                    num,
                    group,
                    act_id), by = "Result_UID")%>%
  group_by(group) %>%
  mutate(code = ifelse(row_number() > 1, 46, ""),
         rational = ifelse(code == 46, "Suspected Duplicate", "" ))

temp_depth <- temp_all_data %>%
  right_join(select(temp_diff, 
                    Result_UID,
                    num,
                    group,
                    act_id),
             by = "Result_UID") %>%
  group_by(group) %>%
  mutate(is_highest_QL = ifelse(QualifierAbbr == min(QualifierAbbr), 'Yes', 'No'),
         num_QL_levels = n_distinct(is_highest_QL, na.rm = FALSE),
         drop_DQL = ifelse(num_QL_levels > 1 & is_highest_QL == 'No', 'Yes', "No"),
         code = ifelse(drop_DQL == 'Yes', 52, "" ),
         rational = ifelse(drop_DQL == "Yes", "Multiple results at datetime and higher DQL exists", ""))


DBI::dbDisconnect(IR.sql)

# create xlsx doc ---------------------------------------------------------





#####################################################################################
## Create Workbook object and add worksheets
wb <- createWorkbook()

## Add worksheets
addWorksheet(wb, "DO suspect rep, depth, x sec")
addWorksheet(wb, "DO suspect duplicate")
addWorksheet(wb, "Temp suspect rep, depth, x sec")
addWorksheet(wb, "Temp suspect duplicate")
addWorksheet(wb, "pH suspect rep, depth, x sec")
addWorksheet(wb, "pH suspect duplicate")

# add data
writeData(wb, "DO suspect rep, depth, x sec", DO_depth, rowNames = FALSE)
writeData(wb, "DO suspect duplicate", DO_dups, rowNames = FALSE)

writeData(wb, "Temp suspect rep, depth, x sec", temp_depth, rowNames = FALSE)
writeData(wb, "Temp suspect duplicate", temp_dups, rowNames = FALSE)

writeData(wb, "pH suspect rep, depth, x sec", pH_depth, rowNames = FALSE)
writeData(wb, "pH suspect duplicate", ph_dups, rowNames = FALSE)

saveWorkbook(wb, "suspect_data.xlsx", overwrite = TRUE)


######################################################################################
######################################################################################
######################################################################################
######################################################################################

# Calculate thresholds ----------------------------------------------------

# Do ----------------------------------------------------------------------




reviewed_DO_data <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/suspect_data v3.xlsx",
                             sheet = 'DO suspect rep, depth, x sec')

DO_data_thresholds <- reviewed_DO_data %>%
  mutate(Result = as.numeric(Result)) %>%
  filter(code == "-999") %>%
  group_by(group) %>%
  mutate(threshold_num = n(),
         min = min(Result),
         max = max(Result),
         delta = max(Result) - min(Result))


test <- DO_data_thresholds %>%
  ungroup() %>%
  filter(threshold_num == 1) %>%
  select(Result_UID)

hist_data <- DO_data_thresholds %>%
  filter(!Result_UID %in% test$Result_UID)

ggplot(data = hist_data) +
  geom_histogram(aes(x =delta ), bins = 100) +
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .90), color = "90th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .50), color = "50th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .10), color = "10th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .75), color = "75th Percentile"))+
  geom_vline(aes(xintercept = 1, color = "DQL = B Criteria"), size = 1.5) +
  scale_y_continuous(breaks=seq(0,150,5),expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0,8,0.5), expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "DO Delta", subtitle = "Delta DO within duplicate groups")+
  theme(panel.grid.minor = element_line(linetype = "blank"))


ggsave("Delta_DO_hist.png")


ggplot(data = hist_data)+
  ggplot2::stat_ecdf(ggplot2::aes(x = delta )) +
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .90), color = "90th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .50), color = "50th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .10), color = "10th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .75), color = "75th Percentile"))+
  geom_vline(aes(xintercept = 1, color = "DQL = B Criteria"), size = 1.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0,8,0.5), expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "DO cumulative frequency", subtitle = "Delta DO within duplicate groups") +
  labs(y = "Cumulative Frequency")

ggsave("Delta_DO_cumulative_freq.png")
# pH ----------------------------------------------------------------------

reviewed_DO_data <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/suspect_data v3.xlsx",
                              sheet = 'pH suspect rep, depth, x sec')

DO_data_thresholds <- reviewed_DO_data %>%
  mutate(Result = as.numeric(Result)) %>%
  filter(code == "-999") %>%
  group_by(group) %>%
  mutate(threshold_num = n(),
         min = min(Result),
         max = max(Result),
         delta = max(Result) - min(Result))


test <- DO_data_thresholds %>%
  ungroup() %>%
  filter(threshold_num == 1) %>%
  select(Result_UID)

hist_data <- DO_data_thresholds %>%
  filter(!Result_UID %in% test$Result_UID)

ggplot(data = hist_data) +
  geom_histogram(aes(x =delta ), bins = 100) +
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .90), color = "90th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .50), color = "50th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .10), color = "10th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .75), color = "75th Percentile"))+
  geom_vline(aes(xintercept = 0.5, color = "DQL = B Criteria"), size = 1.5) +
  scale_y_continuous(breaks=seq(0,150,5),expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0,8,0.2), expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "pH Delta", subtitle = "Delta pH within duplicate groups") + 
  theme(panel.grid.minor = element_line(linetype = "blank"))


ggsave("Delta_pH_hist.png")


ggplot(data = hist_data)+
  ggplot2::stat_ecdf(ggplot2::aes(x = delta )) +
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .90), color = "90th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .50), color = "50th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .10), color = "10th Percentile"))+
  geom_vline(aes(xintercept = quantile(hist_data$delta, probs = .75), color = "75th Percentile"))+
  geom_vline(aes(xintercept = 0.5, color = "DQL = B Criteria"), size = 1.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0,8,0.2), expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "pH cumulative frequency", subtitle = "Delta pH within duplicate groups") +
  labs(y = "Cumulative Frequency")
                      

ggsave("Delta_pH_cumulative_freq.png")
