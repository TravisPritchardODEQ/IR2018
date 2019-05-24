library(lubridate)




# Create dataframe out of data returned from temp data script
temp_asessment <- function(df){

temp_analysis <- df %>%
  mutate(# Add columns for Critcal period start and end date
         Crit_period_start = mdy(paste0("7/1/",year(SampleStartDate))),
         Cirt_period_end = mdy(paste0("9/30/",year(SampleStartDate))),
         # Append spawn start and end dates with year
         Start_spawn = ifelse(!is.na(SpawnStart), paste0(SpawnStart,"/",year(SampleStartDate)), NA ) ,
         End_spawn = ifelse(!is.na(SpawnEnd), paste0(SpawnEnd,"/",year(SampleStartDate)), NA ),
         # Make spwnmn start and end date date format
         Start_spawn = mdy(Start_spawn),
         End_spawn = mdy(End_spawn),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         End_spawn = if_else(End_spawn < Start_spawn & SampleStartDate >= End_spawn, End_spawn + years(1), # add a year if in spawn period carrying to next year
                             End_spawn),
         Start_spawn = if_else(End_spawn < Start_spawn & SampleStartDate <= End_spawn, Start_spawn - years(1), # subtract a year if in spawn period carrying from previous year
                               Start_spawn),
         SampleStartDate = ymd(SampleStartDate), 
         # Flag for results in critical period
         In_crit_period = ifelse(SampleStartDate >=Crit_period_start & SampleStartDate <= Cirt_period_end, 1, 0 ),
         # Print if result is in spawn or out of spawn
         Spawn_type = ifelse((SampleStartDate >= Start_spawn & SampleStartDate <= End_spawn & !is.na(Start_spawn)),  "Spawn", "Not_Spawn"),
         # Flag if result violates standard,  use 13 for during spawn dates, else use criteria
         year_round_Violation = ifelse(Result_cen > Temp_Criteria, 1, 0),
                  # Flag for is violation was in spawn period
         Spawn_Violation = ifelse(Spawn_type == "Spawn" & Result_cen > 13, 1, 0 )
         ) %>%
   arrange(SampleStartDate, SampleStartTime) %>%
  filter(!is.na(AU_ID))

print("Writing data tables for review in 'Parameters/Temperature/Data_Review/")

IR_export(temp_analysis, "Parameters/Temperature/Data_Review", "Temperature", "data" )





# Year_round analysis -----------------------------------------------------


# Create list for getting data out of loop
Au_review_list <- list()


# Assess by AU ------------------------------------------------------------




# Consider 1 AU at a time
for (i in 1:length(unique(temp_analysis$AU_ID))){
  
  # Create dataframe (Review_AU) containing only 1 AU
  AU_4review <- unique(temp_analysis$AU_ID)[i]
  
  print(paste("Assesing AU:", AU_4review, "-", i, "of", length(unique(temp_analysis$AU_ID)) ))
  
  # Filter to get a dataframe of only the AU currently being assessed
  Review_AU <- temp_analysis %>%
    filter(AU_ID == AU_4review) 
  
  
  pb <- txtProgressBar(min = 0, max = nrow(Review_AU), style = 3)
  
  # The for loop below looks at earch result, one at a time and creates a dataframe
  # for each result containing all the results for the preceeding 3 years. 
  # For example, if the result is for 1/1/2017, it will be a dataframe of
  # all results for that AU between 1/1/2014 - 1/1/2017
  # It then creates 4 new columns that are created in the Review_AU dataframe:
  #     Violations_3yr is the number of violations in that 3 year period
  #     DISCONTINUED - Violations_in_Spawning is the number of violations in that 3 year period
  #           that are during spawning periods - DISCONTINUED
  #     Samples_in_crit_period are the number of samples in that 3 year period
  #          that are during the critical period
  #     samples_in_spawn_period are the number of samples in that 3 year period
  #          that are during the spawn period
  
  for(j in 1:nrow(Review_AU)){
    
    
    start3yr <- Review_AU$SampleStartDate[j] - years(3)
    end3yr <- Review_AU$SampleStartDate[j]
    
    period3yr <- Review_AU %>%
      filter(between(SampleStartDate,start3yr, end3yr ))
    
    num_violations = sum(period3yr$year_round_Violation)
    samples_crit_period <- sum(period3yr$In_crit_period)
    samples_spawn <- nrow(subset(period3yr, Spawn_type == "Spawn"))
    spawn_violations <- sum(period3yr$Spawn_type == "Spawn" & period3yr$Spawn_Violation == 1)
    
    
    Review_AU[j,"Violations_3yr"] <- num_violations
    Review_AU[j,"Violations_Spawning_3yr"] <- spawn_violations
    Review_AU[j,"Samples_in_crit_period"] <- samples_crit_period
    Review_AU[j,"samples_in_spawn_period"] <- samples_spawn
    
    setTxtProgressBar(pb, j)
    
  }
  
  # Review_AU gets moved to this list so we can get data out of the loop
  # each element in the list will be the data for each AU
  Au_review_list[[i]] <- Review_AU
 
  close(pb)
  
}






print("Assessment Complete. Beginning Year round Categorization")

# get the data out of the list and create a dataframe
reviewed_data <- bind_rows(Au_review_list) %>%
  arrange(AU_ID, SampleStartDate)

#Remove the list from the environment, to free up memory
rm(Au_review_list)



# Year round categorization -----------------------------------------------
  
# This is the where the IR categories get assigned
Temp_IR_categories <- reviewed_data %>%
    group_by(AU_ID) %>%
    # Sum the total violations and spawning violations by AU
    # So we have a record of total violations over the assessment period
    mutate(total_violations = sum(year_round_Violation)
           #,Spawn_Violation_count = sum(Spawn_Violation)
           ) %>%
    arrange(SampleStartDate) %>%
    # This bit gives us the all rows that match the maximum (and minimum but we drop that later)
    # number of violations in a 3 year period (Violations_3yr). Since we are looking at cat5
    # being 2 in 3 years, if we filter down to 3 year period with the max violations, we
    # can see if the AU shoudl be listed. 
    filter(Violations_3yr %in% range(Violations_3yr)) %>%
    # Create summary of the data needed to make IR categorization determinations. 
    # basically this is so we can see if there are any 3 year periods with 2 or more
    # violations, and see if we have at any point in the assessmnet window any results 
    # in the critical period or the spawn period. We also get the total violations, and 
    # total spawn violations.
    summarise(OWRD_Basin = first(OWRD_Basin), 
              data_period_start = min(SampleStartDate),
              data_period_end = max(SampleStartDate),
              max_violations_3yr = max(Violations_3yr),
              total_violations = first(total_violations),
              #total_Spawn_Violation_count = first(Spawn_Violation_count),
              max_3yr_results_in_crit_period = max(Samples_in_crit_period),
             # max_3yr_results_in_spawn_period = max(samples_in_spawn_period)
             ) %>%
    # Assign to IR categories. 
    #      If there is any 3 year rolling period with 2 or more violations - Cat 5
    #      If all the 3 rolling year periods have less than 2 violations, and there are never any results
    #           in the critical periods or spawn periods - Cat 3
    #      If the three year period with the maximum number of violations has example 1 violation, than Cat3B
    #      Otherwise Category 2
    mutate(IR_category =  case_when(max_violations_3yr >= 2 ~ "Cat5",
                                    max_violations_3yr < 2 & max_3yr_results_in_crit_period == 0 ~ "Cat3",
                                    max_violations_3yr == 1 ~ "Cat3B",
                                    TRUE ~ 'Cat2'),
           Period = "Year round") %>%
    select(AU_ID,Period, OWRD_Basin, data_period_start, data_period_end, IR_category, total_violations, max_violations_3yr, max_3yr_results_in_crit_period)

print(" Year Round Categorization Complete")



# Spawning categorization -------------------------------------------------


print("Start spawning categorization")


# This is the where the IR categories get assigned
Temp_Spawn_IR_categories <- reviewed_data %>%
  filter(Spawn_type == "Spawn") %>%
  group_by(AU_ID) %>%
  # Sum the total violations and spawning violations by AU
  # So we have a record of total violations over the assessment period
  mutate(total_Spawn_violations = sum(Spawn_Violation)) %>%
  arrange(SampleStartDate) %>%
  # This bit gives us the all rows that match the maximum (and minimum but we drop that later)
  # number of violations in a 3 year period (Violations_3yr). Since we are looking at cat5
  # being 2 in 3 years, if we filter down to 3 year period with the max violations, we
  # can see if the AU shoudl be listed. 
  filter(Violations_Spawning_3yr %in% range(Violations_Spawning_3yr)) %>%
  # Create summary of the data needed to make IR categorization determinations. 
  # basically this is so we can see if there are any 3 year periods with 2 or more
  # violations, and see if we have at any point in the assessmnet window any results 
  # in the critical period or the spawn period. We also get the total violations, and 
  # total spawn violations.
  summarise(OWRD_Basin = first(OWRD_Basin), 
            data_period_start = min(SampleStartDate),
            data_period_end = max(SampleStartDate),
            max_spawn_violations_3yr = max(Violations_Spawning_3yr),
            total_spawn_violations = first(total_Spawn_violations),
            max_3yr_results_in_spawn_period = max(samples_in_spawn_period)
  ) %>%
  # Assign to IR categories. 
  #      If there is any 3 year rolling period with 2 or more violations - Cat 5
  #      If all the 3 rolling year periods have less than 2 violations, and there are never any results
  #           in the critical periods or spawn periods - Cat 3
  #      If the three year period with the maximum number of violations has example 1 violation, than Cat3B
  #      Otherwise Category 2
  mutate(IR_category =  case_when(max_spawn_violations_3yr >= 2 ~ "Cat5",
                                  max_spawn_violations_3yr == 1 ~ "Cat3B",
                                  max_spawn_violations_3yr < 1 ~ 'Cat2'),
         Period = 'Spawn') %>%
  select(AU_ID, Period,  OWRD_Basin, data_period_start, data_period_end, IR_category, total_spawn_violations, max_spawn_violations_3yr)




# Cat 3 analysis ----------------------------------------------------------

#select from database AU's with any sort of temperature results

# Get DO and temp data from IR_database to calculate percent sat --------
con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

all_temperature_data <- "SELECT [AU_ID], 
OWRD_Basin, 
min([SampleStartDate]) as data_period_start, 
max([SampleStartDate]) as data_period_end

FROM [IntegratedReport].[dbo].[InputRaw]
where Char_Name = 'Temperature, water' and Statistical_Base = 'Mean'
group by [AU_ID], OWRD_Basin
"

all_temp_AUs_query <-  glue::glue_sql(all_temperature_data, .con = con)
all_temp_AUs <- DBI::dbGetQuery(con, all_temp_AUs_query)
DBI::dbDisconnect(con)

AUs_with_no_7DADM <- all_temp_AUs %>%
  filter(!AU_ID %in% unique(temp_analysis$AU_ID),
         AU_ID != "") %>%
  mutate(IR_category = "Cat3",
         Period = "Year round",
         analysis_comment = "Data insufficient to calculate 7DADM value")%>%
  mutate(data_period_start = ymd(data_period_start),
         data_period_end = ymd(data_period_end))



# Put everything together -------------------------------------------------


all_temp_categories <- bind_rows(Temp_IR_categories, Temp_Spawn_IR_categories, AUs_with_no_7DADM) %>%
  arrange(AU_ID) 

return(all_temp_categories)
}