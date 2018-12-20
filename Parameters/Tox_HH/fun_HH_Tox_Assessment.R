library(lubridate)

fun_Tox_HH_analysis <- function(df){ 


tox_HH_assesment <- df %>%
  mutate(violation = ifelse(Result_cen > crit, 1, 0 )) 



write.csv(tox_HH_assesment, "Parameters/Tox_HH/Data_Review/HH_tox_analysis.csv")
  
tox_HH_categories <- tox_HH_assesment %>%
  group_by(AU_ID, Pollutant, Sample_Fraction,Crit_Fraction) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            crit = max(crit),
            num_samples = n(),
            num_violations = sum(violation),
            geomean = geo_mean(Result_cen)) %>%
  ungroup() %>%
  group_by(AU_ID, Pollutant) %>%
  mutate(num_fraction_types =  n(),
         IR_category = ifelse(num_samples >= 3 & geomean >= crit, "Cat5", 
                                     ifelse(num_samples < 3 & num_violations >= 1, "Cat3B", 
                                            ifelse(num_samples < 3 & num_violations == 0, "Cat3", "Cat2" )))) %>%
  arrange(AU_ID, Pollutant)
 
#write tablehere
}

# To do - 
#Figure out fractions piece
# make sure units are correct in data pull