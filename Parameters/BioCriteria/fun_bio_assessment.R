biodata <- biodata %>% 
  filter(Qualifier == "DQL=A") %>%
  filter(ID == "% Taxa Loss")

#### Marine Western Coastal Forest
MWCF_AU_sum = biodata %>%
  filter(Eco2 == "MWCF") %>%
  group_by(AU_ID) %>%
  summarise(num_Samples = n(),
            n_over20PTL = sum(Score >= 20),
            n_15to20PTL = sum(Score > 15 & Score < 20),
            n_9to14PTL = sum(Score > 9 & Score < 14),
            n_less8PTL = sum(Score <= 8)) %>%
  mutate(IR_Cat = ifelse(num_Samples == 1 & n_over20PTL >=1 | num_Samples >= 2 & n_over20PTL >=1 | num_Samples >= 2 & n_15to20PTL >=1,"Cat5",
                         ifelse(num_Samples == 1 & n_15to20PTL >=1,"Cat3B",
                                ifelse(n_9to14PTL >= 1,"Cat3C",
                                       ifelse(n_less8PTL >= 1,"Cat2","")))))


#### Western Cordillera and Columbia Plateau 
WC_AU_sum = biodata %>%
  filter(Eco2 == "WC"|Eco2 == "COLD DESERTS") %>%
  group_by(AU_ID) %>%
  summarise(num_Samples = n(),
            n_over20PTL = sum(ID >= 20),
            n_15to20PTL = sum(ID > 15 & ID < 20),
            n_9to14PTL = sum(ID > 9 & ID < 14),
            n_less8PTL = sum(ID <= 8)) %>%
  mutate(IR_Cat = ifelse(num_Samples == 1 & n_over20PTL >=1 | num_Samples >= 2 & n_over20PTL >=1 | num_Samples >= 2 & n_15to20PTL >=1,"Cat5",
                         ifelse(num_Samples == 1 & n_15to20PTL >=1,"Cat3B",
                                ifelse(n_15to20PTL >=1,"Cat3C",
                                       ifelse(n_less8PTL >=1,"Cat2","")))))
