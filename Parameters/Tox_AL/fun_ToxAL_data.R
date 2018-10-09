require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)

#testing 
database = "IR 2018"


ALTox_data <- function(database) {
  print("Fetch AL Toxic data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  IR.sql <-  odbcConnect(database, case="nochange")
  
  
  
  # Get data from IR database where wqstd_code = 1 and ResStatusName = Final
  # Join with Crit_Bact to get bacteria Criteria
  Results_import <- sqlFetch(IR.sql,"VW_ToxAL")
  
  odbcClose(IR.sql)
  