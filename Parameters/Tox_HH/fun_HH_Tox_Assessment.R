library(lubridate)

#fun_Tox_HH_analysis <- function(df){} 


tox_HH_assesment <- Results_censored_tox_HH %>%
  mutate(violation = ifelse(Result_cen > crit, 1, 0 )) 



#Write table here
  
tox_HH_categories <- tox_HH_assesment %>%
  group_by(AU_ID, Pollutant, SampleFractName,Crit_Fraction) %>%
  summarise(crit = max(crit),
            num_samples = n(),
            num_violation = sum(violation),
            geomean = geo_mean(Result_cen)) %>%
  ungroup() %>%
  group_by(AU_ID, Pollutant) %>%
  mutate(num_fraction_types =  n(),
         critical_num = excursions_conv(num_samples),
         IR_category = ifelse(num_violation >= critical_num, "Cat5 (Binomial)", 
                              ifelse(num_samples >= 3 & geomean >= crit, "Cat5 (Geomean)", 
                                     ifelse(num_samples < 3 & num_violation >= 1, "Cat3B", 
                                            ifelse(num_samples < 3 & num_violation == 0, "Cat3", "Cat2" ))))) %>%
  arrange(AU_ID, Pollutant)
 
#write tablehere


# To do - 
#Figure out fractions piece
# make sure units are correct in data pull