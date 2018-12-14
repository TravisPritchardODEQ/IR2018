require(tidyverse)
require(IRlibrary) 
library(odbc)
library(DBI)
library(glue)

Pentachlorophenol_data <- function(database){

print("Fetch bacteria data from IR database")

#open connection to database
con <- DBI::dbConnect(odbc::odbc(), database)

#Build query language to get Pentachlorophenol data out. this grabs the IR 2018 db view [dbo].[VW_Pentachlorophenol]

db_qry <- DoSatqry <- glue::glue_sql( "SELECT *
  FROM [IntegratedReport].[dbo].[VW_Pentachlorophenol]", .con = con)

# Send query to database and return with the data
Results_import <-  DBI::dbGetQuery(con, db_qry) 


#Create a vector of monitoring locations with Pentachlorophenol data. This list is used as a filter for the pH query
mlocs <- unique(Results_import$MLocID)


#Create query for the pH data. The 'MLocID in ({mlocs*})' part automatically adds 
#all the mlocs vector elements to the query 

ph_qry <- glue::glue_sql("SELECT [MLocID]
,[SampleStartDate]
,[IRResultNWQSunit] AS 'pH'
,[Result_Depth]
FROM [IntegratedReport].[dbo].[InputRaw]
WHERE Pollu_ID = '124' AND (Statistical_Base IS NULL)
      AND MLocID in ({mlocs*})", .con = con)


#Send to db and return with the data
Results_pH <- DBI::dbGetQuery(con, ph_qry)


# Close database connection
DBI::dbDisconnect(con)


# If there are multiple pH data points in a day, we only use the first one. 
# This section just selects the first pH value for a given mloc, date, and depth
single_day_ph <- Results_pH %>%
  group_by(MLocID, SampleStartDate, Result_Depth ) %>%
  summarise(pH = first(pH)) %>%
  ungroup()

#Join the pH data to the Pentachlorophenol data and calculate the criteria
joined_data <- Results_import %>%
  left_join(single_day_ph, by = c("MLocID","SampleStartDate", "Result_Depth" )) %>%
  mutate(CMC_crit = exp(1.005*pH-4.869),
         CCC_crit = exp(1.005*pH-5.134))


#Run data censoring here
data_censored <- Censored_data(joined_data, crit = CMC_crit)

#Return censored data from the function 
return(data_censored)

}

