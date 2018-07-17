library(lubridate)
library(ggthemes)

# This script pull sout the NWIS data marked as provisional so we can flag it and send to USGS 
# for advice. 



# Show imported provisional grades ----------------------------------------



provisional_temp_import <- nwis.sum.stats.temp %>%
  filter(startsWith(Wtemp_Max_cd, "P"))


provisional_DO_import <- nwis.sum.stats.DO %>%
  filter(startsWith(DO_Max_cd, "P"))

write.csv(provisional_temp_import, "Data Sources/NWIS_provisional_data_temp.csv")
write.csv(provisional_DO_import, "Data Sources/NWIS_provisional_data_DO.csv")


# Show AWQMS provisional grades -------------------------------------------


provisional_temp_AWQMS <- nwis.sum.stats.temp.AWQMS %>%
  mutate(year = year(ActStartDate),
         provis = ifelse(startsWith(Qualcd, "P"), 1, 0 )) %>%
  group_by(year) %>%
  summarise(num = n(),
            provis_num = sum(provis),
            provis_per = provis_num/num*100)


provisional_DO_AWQMS <- nwis.sum.stats.DO.AWQMS %>%
  mutate(year = year(ActStartDate),
         provis = ifelse(startsWith(Qualcd, "P"), 1, 0 )) %>%
  group_by(year) %>%
  summarise(num = n(),
            provis_num = sum(provis),
            provis_per = provis_num/num*100)

ggplot(data = provisional_DO_AWQMS, aes(x=factor(year), y= provis_per))+
  geom_bar(stat = "identity") +
  xlab("year")+
  ylab("Percent Provisional")+
  theme_minimal()+
  theme(  panel.grid.major.x = element_blank())