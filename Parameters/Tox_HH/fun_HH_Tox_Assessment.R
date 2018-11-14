library(lubridate)

#fun_Tox_HH_analysis <- function(df){} 


tox_HH_assesment <- Results_censored_tox_HH %>%
  mutate(violation = ifelse(Result_cen > crit, 1, 0 )) 


#Figure out where we have 3 years of consecutive data 
  # add column for year
  # Pare down datatable to only AU, charcateristic, and year
  # Group by and summarze to get unique values for AU, chr. and year
  # Regoup by AU and CHY
  # Order by AU, Chr, and year
  # Calculate difference in years between the year from the grouped year above
    # This mainitaings groups, i.e. we will not be comparing different chrs or AUs
  # If the diff value for a row is 1, and the row above is also 1, then that is a consecutive three year period
consecutive_yrs <- tox_HH_assesment %>%
  mutate(year = year(ActStartD)) %>%
  select(AU_ID, ChrName, year) %>%
  group_by(AU_ID, ChrName, year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(AU_ID, ChrName) %>%
  arrange(AU_ID, ChrName, year) %>%
  mutate(diffs = c(0, diff(year))) %>%
  mutate(consecutive_3yr = ifelse((lag(diffs , 1) == 1)  & diffs == 1, 1, 0 ) ) 

#Write table here
  
tox_HH_categories <- tox_HH_assesment %>%
  left_join(select(consecutive_yrs, AU_ID, ChrName, consecutive_3yr), by = c("AU_ID", "ChrName")) %>%
  group_by(AU_ID, Pollutant) %>%
  summarise(consecutive_3yr = max(consecutive_3yr),
            num_samples = n(),
            num_violation = sum(violation)) %>%
  mutate(critical_num = excursions_conv(num_samples),
         IR_category = ifelse(consecutive_3yr == 1 & num_violation >= critical_num, "Cat5", 
                              ifelse(consecutive_3yr == 0 & num_violation >= 2, "Cat5", 
                                     ifelse(consecutive_3yr == 0 & num_violation == 1, "Cat3B", "ERROR" ))))
 



# To do - 
#Figure hout how to implement the If data are not available in 3 consecutive years
#Figure out a 3 and 3b listing
#Figure out fractions piece