#Install packages needed for IR2018. This will only install packaages not already installed



IR.packages <- c("tidyverse", "readxl", "lubridate", "openxlsx", "dataRetrieval", "rgdal","RODBC",
                 "odbc", "glue", "DBI", "zoo")
new.packages <- IR.packages[!(IR.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)