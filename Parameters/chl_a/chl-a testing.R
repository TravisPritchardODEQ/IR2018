require(rgdal)
require(RODBC)
library(tidyverse)
library(IRlibrary)
library(lubridate)
library(openxlsx)
library(zoo)



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

testing <- chl_data_import %>%
   mutate( month = month(ActStartD),
          yrfromstart = year(ActStartD) - 2008,
          monthfromstart = month + 12*yrfromstart) %>%
  group_by(MLocID, monthfromstart) %>%
  summarise(monthaverage = mean(Result4IR))

  
  # convert to data.table
DT <- data.table(testing)

# make sure `date` is in fact a date and not a string
DT[, ActStartD := as.Date(ActStartD)]

# re order
DT <- setkey(DT[order(monthfromstart)], MLocID)

# compute diffs
DT[, diffs := c(0, diff(monthfromstart)), by=MLocID]


## We will use cumsum.  Anything greater than 1, should be reset to 0
DT[diffs > 1, diffs := 9999]
#DT[, diffs := diffs + 1]


# fix duplicate dates
DT[, diffs := max(diffs), by=list(MLocID, ActStartD)]

# Reset as dataframe
DT <- as.data.frame(DT)

detach_package(data.table)

testing3 <- DT %>%
  mutate(consecutive3 = ifelse((lag(diffs , 2) != 9999) & (lag(diffs , 1) != 9999)  & diffs!= 9999, 1, 0 ) ) %>%
  filter(consecutive3 == 1)

month()

testing4 <- testing3 %>%
  select(OrgID, MLocID, ActStartD, ActStartT, Result4IR, ResultOp4IR, 
         month, yrfromstart,monthfromstart,diffs, consecutive3  )



# 

testing <- chl_data_import %>%
  mutate( month = month(ActStartD),
          yrfromstart = year(ActStartD) - 2008,
          monthfromstart = month + 12*yrfromstart) %>%
  group_by(MLocID, monthfromstart) %>%
  summarise(monthaverage = mean(Result4IR))

DT <- data.table(testing)

# re order
DT <- setkey(DT[order(monthfromstart)], MLocID)

# compute diffs
DT[, diffs := c(0, diff(monthfromstart)), by=MLocID]

## We will use cumsum.  Anything greater than 1, should be reset to 0
DT[diffs > 1, diffs := 9999]
#DT[, diffs := diffs + 1]



# Reset as dataframe
DT <- as.data.frame(DT)


testing3 <- DT %>%
  mutate(consecutive3 = ifelse((lag(diffs , 2) != 9999) & (lag(diffs , 1) != 9999)  & diffs!= 9999, 1, 0 ) )

testing3 <- DT %>%
  group_by(MLocID) %>%
  mutate(row = row_number()) %>%
  mutate(consecutive3 = ifelse((lag(diffs , 1) != 9999)  & diffs!= 9999 & row >= 3, 1, 0 ) )


avgs <- testing3 %>%
  group_by(MLocID) %>%
  mutate(avg.3.mo = rollapply(data = monthaverage,
                              width = 3,
                              FUN = mean,
                              align = 'right',
                              fill= NA,
                              na.rm = T))


avgs <- testing3 %>%
  group_by(MLocID) %>%
  mutate(avg.3.mo = ifelse(consecutive3 == 1, rollmean(monthaverage,
                                                       3,
                                                       align = 'right',
                                                       fill= NA,
                                                       na.rm = T), "" ) )
