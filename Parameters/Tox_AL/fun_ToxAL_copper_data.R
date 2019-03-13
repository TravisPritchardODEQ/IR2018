require(tidyverse)
require(IRlibrary) 
library(odbc)
library(DBI)
library(glue)


# This function builds an export table to run the copper BLM. It includes 
# all necessary ancillary data. 

#Where there are multiple fractions, or multiple names for constiuents, this 
#script will export the maximums. 

Copper_data <- function(database){


  print("Fetch Copper data from IR database")
  
  #open connection to database
con <- DBI::dbConnect(odbc::odbc(), database)
  
  #Build query language to get Pentachlorophenol data out. this grabs the IR 2018 db view [dbo].[VW_Pentachlorophenol]
  
db_qry <- glue::glue_sql( "SELECT *
  FROM [IntegratedReport].[dbo].[VW_Copper]", .con = con)
  
  # Send query to database and return with the data
Results_import <-  DBI::dbGetQuery(con, db_qry)
  
print(paste("Returned", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations"))
  
  #Create a vector of monitoring locations with Pentachlorophenol data. This list is used as a filter for the pH query
mlocs <- unique(Results_import$MLocID)

print("Fetch ancillary data from IR database")
ancillary_qry <- glue::glue_sql("SELECT [MLocID]
,[chr_uid]
,[SampleStartDate]
,[Char_Name]
,[Result_Unit]
,[Unit_UID]
,[Sample_Fraction]
,[IRResultNWQSunit]
,[Result_Depth]
FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
WHERE chr_uid in ('2849', '1648', '727', '1244', 1802, 1709, 1827, 773, 544, 100331, 1097, 1099, 2174, 2982) 
      AND (Statistical_Base IS NULL)
      AND MLocID in ({mlocs*})", .con = con)

Results_ancillary <- DBI::dbGetQuery(con, ancillary_qry)



# Close database connection
DBI::dbDisconnect(con)


spread <- Results_ancillary %>%
  mutate(Char_Name = ifelse(chr_uid %in% c(544, 100331), 'Alkalinity', 
                            ifelse(chr_uid %in% c(1097, 1099), 'Hardness', 
                                   ifelse(chr_uid == 2174 & Sample_Fraction == "Total" , 'TOC', 
                                          ifelse(chr_uid == 2174 & Sample_Fraction == "Dissolved", 'DOC', Char_Name ))))) %>%
  mutate(IRResultNWQSunit = ifelse(IRWQSUnitName == 'ug/l', IRResultNWQSunit / 1000, IRResultNWQSunit),
         Result_Unit = ifelse(IRWQSUnitName == 'ug/l', "mg/L", Result_Unit)) %>%
  #mutate(Char_Name = paste(Char_Name, "-", Result_Unit)) %>%
  group_by(MLocID, SampleStartDate,Char_Name, Result_Depth) %>%
  summarise(result = max(IRResultNWQSunit)) %>%
  arrange(MLocID, SampleStartDate) %>%
  spread(key = Char_Name, value = result)



copper_data <- Results_import %>%
  left_join(spread, by = c("MLocID", "SampleStartDate", "Result_Depth")) %>%
  arrange(MLocID, SampleStartDate, SampleStartTime)

write.csv(copper_data, "Parameters/Tox_AL/Data_Review/Copper_data_4_BLM.csv", row.names = FALSE)

}