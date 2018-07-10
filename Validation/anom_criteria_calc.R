library(tidyverse)
library(readxl)


#read data into R
OWQI_data <- read.xlsx("Validation/OWQI_DATA_1980to2017.xlsx")

#create anom_crit table
anom_crit <- OWQI_data %>%
  #make table long format 
  gather(key = char, r,  temp:fecal) %>%
  #remove rows with now results
  filter(!is.na(r)) %>%
  group_by(HUC4, char) %>%
  #create 99th and 1st percentile and summarize data
  summarise(per99 = as.numeric((quantile(r, c(0.99, 0.01))["99%"])),
            per1 = as.numeric((quantile(r, c(0.99, 0.01))["1%"]))) %>%
  #add information rows 
  mutate(AmbDatarange = "1980-2017",
         StdRef = "OWQI") %>%
  #change some variable names around
  mutate(char = ifelse(char == "d_o", "do", ifelse(char == "do_sat", "dos", char )))

#save table to be loaded into R later
save(anom_crit, file = "Validation/anom_crit.Rdata" )
