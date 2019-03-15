library(lubridate)
library(odbc)
library(glue)
library(DBI)
library(zoo)
library(IRlibrary)

# clean out exisiting environment
# helps to avoid overwriting
rm(list = ls())

options(scipen=999)

# Choose submitted file to generate stats on 
filepath <- file.choose()
Results_import <- read_excel(filepath)

#Reformat table from an AWQMS import file to something that's easier to work with here
# remove temperature data since we are going to do a DO analysis and DO sat is provided
#Filter out rejected data
#rename DO and DOSat value sto make it easier to transform from 'long' to 'wide' format 
#add spawn start and end dates
DO_results <- Results_import %>%
  filter(charID != 'Temperature, water',
         ResultStatusID != 'Rejected') %>%
  rename(SampleStartDate = ActStartDate,
         MLocID = Monitoring.Location.ID,
         Statistical_Base = StatisticalBasis) %>%
  mutate(charID = ifelse(charID == 'Dissolved oxygen (DO)', "DO_Conc", "DO_Sat" ),
         Statistical_Base = ifelse(Statistical_Base == '7DMADMean', '7DADMean', 
                                   ifelse(Statistical_Base == 'Daily Minimum', 'Minimum', Statistical_Base )), 
         Start_spawn = paste0("9/1/", year(SampleStartDate)),
         End_spawn = paste0("6/15/", year(SampleStartDate)),
         Start_spawn = mdy(Start_spawn),
         End_spawn = mdy(End_spawn),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         End_spawn = if_else(End_spawn < Start_spawn & SampleStartDate >= End_spawn, End_spawn + years(1), # add a year if in spawn period carrying to next year
                             End_spawn),
         Start_spawn = if_else(End_spawn < Start_spawn & SampleStartDate <= End_spawn, Start_spawn - years(1), # subtract a year if in spawn period carrying from previous year
                               Start_spawn),
         SampleStartDate = ymd(SampleStartDate),
         in_spawn = ifelse(SampleStartDate >= Start_spawn & SampleStartDate <= End_spawn & !is.na(Start_spawn), 1, 0 )) %>%
  select(-r_units) %>%
  spread(key = charID, value = Result) %>%
  filter(in_spawn == 1)





# Continuous --------------------------------------------------------------


#  get counts of number of results per ResultBasesName and AU_ID
Results_spawndates_counts <- DO_results %>%
  group_by(MLocID, Statistical_Base) %>%
  summarise(count = n())


# Data table of results of 7DADMean with more than 15 values
continuous_data_AUs <- Results_spawndates_counts %>%
  filter(Statistical_Base == "7DADMean",
         count >= 15)


# This table is the table of data that will be used for evaulation
continuous_data <- DO_results %>%
  filter(MLocID %in% unique(continuous_data_AUs$MLocID),
         Statistical_Base == "7DADMean" )


# The monitoring locations that have data that meets continuous metrics criteria
continuous_mon_locs <- unique(continuous_data$MLocID)


# Cont spawn analysis -----------------------------------------------------

# Mark result as violation if DO_7D < 11 and the sdosat_7day is < 95
cont_spawn_Do_analysis <- continuous_data %>%
  mutate(Violation = ifelse((DO_Conc < 11.0 &
                               DO_Sat < 95.0) |
                              (DO_Conc < 11.0 &
                                 is.na(DO_Sat)), 1, 0 ))

write.csv(cont_spawn_Do_analysis, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/SFWW/DO_spawn_data.csv", row.names = FALSE)

daily_minimums <- DO_results %>%
  filter(Statistical_Base == "Minimum",
         MLocID %in% unique(cont_spawn_Do_analysis$MLocID)) %>%
  group_by(MLocID) %>%
  summarise(num_below_abs_min = sum(DO_Conc < 9))



# Categorize based on the analysis. 
# this table gets returned in the function
# Summarize violations and number of samples
# If there are 2 of more violations - Cat 5, 
# Else Cat 2
cont_spawn_DO_categories <- cont_spawn_Do_analysis %>%
  group_by(MLocID) %>%
  summarise(num_violations = sum(Violation, na.rm = TRUE)) %>%
  left_join(daily_minimums, by = 'MLocID') %>%
  mutate(category = ifelse(num_violations >= 2 | num_below_abs_min >= 2 , "Cat 5", 
                           "Cat 2" )) #%>%

write.csv(cont_spawn_DO_categories, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/SFWW/DO_spawn_categories.csv", row.names = FALSE)

raw_data <- read_excel("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Information_Requests/SFWW/SFWW_Template_31519.xlsx", sheet = "Results")

raw_graph_data <- raw_data %>%
  rename(MLocID = "Monitoring Location ID",
         SampleStartDate = `Activity Start Date`,
         SampleStartTime = `Activity Start Time`,
         char_id = `Characteristic Name`,
         result = `Result Value`) %>%
  mutate(SampleStartTime = strftime(SampleStartTime, format="%H:%M:%S", tz = "GMT"),
         datetime = paste(SampleStartDate, SampleStartTime),
         datetime = ymd_hms(datetime)) %>%
  filter(char_id == 'Dissolved oxygen (DO)')

ggplot()+
  geom_point(data = raw_graph_data, aes(x = datetime, y = result), alpha = 0.3, color = 'steelblue') +
  geom_line(data = continuous_data, aes(x = ymd_hms(paste(SampleStartDate, "0:00:00")), y = DO_Conc), size = 2) +
  geom_segment(data = continuous_data, aes(x = mdy_hms("09/01/2018 0:00:00"), 
                                           xend = mdy_hms("02/08/2019 0:00:00"), 
                                           y = 11, 
                                           yend = 11 ),
               color = "red",
               linetype = 'longdash') +
  geom_segment(data = continuous_data, aes(x = mdy_hms("05/01/2018 0:00:00"), 
                                           xend = mdy_hms("06/15/2018 23:59:00"), 
                                           y = 11, 
                                           yend = 11 ),
               color = "red",
               linetype = 'longdash') +
  facet_grid(rows =  vars(MLocID)) + 
  theme_bw() +
  coord_cartesian(xlim = c(mdy_hms("05/31/2018 0:00:00"), mdy_hms("02/08/2019 0:00:00"))) +
  labs(x = "Date") +
  theme(strip.background =element_rect(fill="white"))