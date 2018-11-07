

#fun_Tox_HH_analysis <- function(df){} 


tox_HH_assesment <- Results_censored_tox_HH %>%
  mutate(violation = ifelse(Result_cen > crit, 1, 0 )) 

#Write table here

tox_HH_categories <- tox_HH_assesment %>%
  group_by(AU_ID, Pollutant) %>%
  summarise(num_samples = n(),
            num_violation = sum(violation)) %>%
  mutate(critical_num = excursions_conv(num_samples)) %>%
  mutate(IR_category = ifelse(num_violation >= critical_num, "Cat5", "Cat2" ))



# To do - 
#Figure hout how to implement the If data are not available in 3 consecutive years
#Figure out a 3 and 3b listing
#Figure out fractions piece