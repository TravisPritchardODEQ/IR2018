require(rgdal)
require(RODBC)
library(tidyverse)

#disable scientific notation 
options(scipen = 999)


#connect to IR database view as a general user 
IR.sql = odbcConnect('IR 2018')

Results_import <- sqlFetch(IR.sql, "resultsrawWATER")



odbcClose(IR.sql)