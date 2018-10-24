require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)
library(lubridate)
library(openxlsx)



print("Fetch chl a data from IR database")
#connect to IR database view as a general user
# import Temperature data
IR.sql <-   odbcConnect("IR 2018")


# Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
# Join with Crit_Temp to get temperature Criteria and spawn ?
chl_data_import <-    sqlQuery(IR.sql, "SELECT        resultsrawWATER.OrgID, resultsrawWATER.MLocID, resultsrawWATER.MTypeName, resultsrawWATER.ActMediaName, resultsrawWATER.ActStartD, resultsrawWATER.ActStartT, resultsrawWATER.SampleFractName, 
                         resultsrawWATER.ChrName, resultsrawWATER.ResStatusName, resultsrawWATER.Result, resultsrawWATER.Result4IR, resultsrawWATER.ResultOp4IR, resultsrawWATER.ResultComment, 
                           resultsrawWATER.ResultlabComment, WQSTND_Info.wqstd_code, resultsrawWATER.ResultBasesName, resultsrawWATER.ResultTBaseName
FROM            resultsrawWATER INNER JOIN
                           WQSTND_Info ON resultsrawWATER.PolluantID = WQSTND_Info.Pollu_ID
WHERE        (resultsrawWATER.ChrName = 'Chlorophyll a')
Order by  resultsrawWATER.OrgID, resultsrawWATER.MLocID, resultsrawWATER.ActStartD
") 


odbcClose(IR.sql)

testing <- chl_data_import %>%
  mutate( month = month(ActStartD),
         yrfromstart = year(ActStartD) - 2008,
         monthfromstart = month + 12*yrfromstart) %>%
  arrange(MLocID,monthfromstart ) %>%
  group_by(MLocID) %>%
  mutate(diff = cumsum(c(1, diff(monthfromstart)>1)),
         num = rle(diff)$lengths) 

A <- testing$monthfromstart
B <- cumsum(c(1, diff(A)>3))
rle(B)$lengths

# https://stackoverflow.com/questions/17431904/count-the-number-of-consecutive-days-of-activity
library(data.table)

testing <- chl_data_import 
DT <- data.table(testing)

DT[, ActStartD := as.Date(ActStartD)]
DT <- setkey(DT[order(monthfromstart)], MLocID)

DT[, diffs := c(0, diff(monthfromstart)), by=MLocID]

DT[diffs > 1, diffs := 9999]
#DT[, diffs := diffs + 1]

DT[, diffs := max(diffs), by=list(MLocID, ActStartD)]


DT <- as.data.frame(DT)

testing3 <- DT %>%
  mutate(consecutive3 = ifelse((lag(diffs , 2) != 9999) & (lag(diffs , 1) != 9999)  & diffs!= 9999, 1, 0 ) ) %>%
  filter(consecutive3 == 1)