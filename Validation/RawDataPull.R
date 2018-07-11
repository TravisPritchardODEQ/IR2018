require(rgdal)
require(RODBC)
library(tidyverse)

#disable scientific notation 
options(scipen = 999)


#connect to IR database view as a general user 
IR.sql = odbcConnect('IR 2018')

Results_import <- sqlFetch(IR.sql, "resultsrawWATER")



odbcClose(IR.sql)

# Set factors to characters
Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import

bacteria <- Results_import %>%
  filter(ChrName == "Escherichia coli" |ChrName == "Enterococcus")