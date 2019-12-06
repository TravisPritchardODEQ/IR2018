# 12/6/2019 - Travis Pritchard


# This script is for use for generating AWQMS stations import files
# This was created so that we can update AWQMS monitoring stations
# to Assessment units. These files only include stations found in IR 2018 
# InputRaw table. 


# This script pulls the orgID and Mloc ID from IR 2018 inputRaw and 
# joins it to the Stations database VWStationsFinal. It then pulls in
# a file containing the names and descriptions of all the assessment units. 
# These files are joined together and then formatted to match AWQMS 
# import configuration #1226. 

# The script then runs through a for loop which outputs an excel file
# for each organization which contains the stations template for 
# AWQMS

require(rgdal)
require(RODBC)
library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)




#disable scientific notation 
options(scipen = 999)


con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

query <- "SELECT distinct [OrganizationID]
      ,[MLocID]
      
  FROM [IntegratedReport].[dbo].[InputRaw]"

mlocquery <- glue::glue_sql(query, .con = con)
inputraw_monlocs <- DBI::dbGetQuery(con, mlocquery)

DBI::dbDisconnect(con)

#connect to view as a general user 
sta.sql = odbcConnect('Stations')

# Connect to stations database --------------------------------------------

#pull in stations table
Stations = sqlFetch(sta.sql, "VWStationsFinal") 


odbcClose(sta.sql)






# Bring in AU names and descriptions --------------------------------------


AU_names <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/AU_names.csv",
                     stringsAsFactors = FALSE)

# Process the stations ----------------------------------------------------


#join input raw to mlocs

joined_stations <- inputraw_monlocs %>%
  #join to stations
  left_join(select(Stations, -OrgID)) %>%
  #join to AU names to get name and description
  left_join(select(AU_names, AU_ID, AU_Name, AU_Description), by = c("AU_ID")) %>%
  mutate(country = "US",
         loc_desc = "") %>%
  select(
    MLocID,
    OrganizationID,
    StationDes,
    loc_desc,
    MonLocType,
    COUNTY,
    STATE,
    country,
    HUC8,
    HUC12,
    TribalLand,
    TribalName,
    Created_Date,
    T_R_S,
    Lat_DD,
    Long_DD,
    Datum,
    CollMethod,
    MapScale,
    Comments,
    WellType,
    WellFormType,
    WellAquiferName,
    WellDepth,
    WellDepthUnit,
    AltLocID,
    AltLocName,
    EcoRegion3,
    EcoRegion4,
    Reachcode,
    GNIS_Name,
    AU_ID,
    AU_Name,
    AU_Description
  )



#run through stations and create am excel for each org


for(i in 1:length(unique(joined_stations$OrganizationID))) {

  print(noquote(paste("Starting file", i, "of", length(unique(joined_stations$OrganizationID)) )))

  
  #get org abriviation to filter with
  organization_abr <-  unique(joined_stations$OrganizationID)[[i]]

  #filter dataframe to only one org
  org_template <-  joined_stations %>%
    filter(OrganizationID == organization_abr)

  #get the description for use in the filename
  organization_desc <- org_template$OrganizationID[[1]]


  class(org_template$HUC12) <- c("NULL", "number")
  class(org_template$Reachcode) <- c("NULL", "number")

  #save the excel file

  
  write.xlsx(org_template, paste0('//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/AWQMS/Station files/',organization_abr, "-Sites.xlsx" ), na = "", asTable = FALSE)

}







