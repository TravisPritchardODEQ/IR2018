library(tidyverse)
library(DBI)
library(openxlsx)


con <- DBI::dbConnect(odbc::odbc(), "IR 2018")


OWRD_query <- glue::glue_sql("SELECT distinct [AU_ID]
                             ,[OrganizationID]
                             ,[MLocID]
                             
                             FROM [IntegratedReport].[dbo].[InputRaw]", .con = con)

Assessed_Stations <- DBI::dbGetQuery(con, "SELECT distinct [AU_ID]
                             ,[OrganizationID]
                             ,[MLocID]
                             
                             FROM [IntegratedReport].[dbo].[InputRaw]")

Assessed_Stations <- Assessed_Stations %>%
  arrange(AU_ID)

write.xlsx(Assessed_Stations, file = "ATTAINS/Rollup/Assessed_stations.xlsx")
