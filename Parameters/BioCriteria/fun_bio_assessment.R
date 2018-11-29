### Assess biocriteria using % taxa loss from the PREDATOR model 

BioCriteria_Assement <- function(df){

bio_A <- BioCriteria %>% 
  filter(Qualifier == "DQL=A")

MWCF_AU_sum = bio_A %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST") %>%
  group_by(AU_ID) %>%
  summarise(num_Samples = n(),
            n_over20PTL = sum(Score >= 20),
            n_15to20PTL = sum(Score >= 15 & Score <= 20),
            n_9to14PTL = sum(Score >= 9 & Score <= 14),
            n_less8PTL = sum(Score <= 8)) %>%
  mutate(IR_Cat = ifelse(num_Samples == 1 & n_over20PTL >=1 | num_Samples >= 2 & n_over20PTL >=1 | num_Samples >= 2 & n_15to20PTL >=1,"Cat5",
                         ifelse(num_Samples == 1 & n_15to20PTL >=1,"Cat3B",
                                ifelse(n_9to14PTL >= 1,"Cat3C",
                                       ifelse(n_less8PTL >= 1,"Cat2","")))))

WC_AU_sum = bio_A %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  group_by(AU_ID) %>%
  summarise(num_Samples = n(),
            n_over27PTL = sum(Score >= 27),
            n_22to27PTL = sum(Score >= 22 & Score <= 26),
            n_8to21PTL = sum(Score >= 8 & Score <= 21),
            n_less7PTL = sum(Score <= 7)) %>%
  mutate(IR_Cat = ifelse(num_Samples == 1 & n_over27PTL >=1 | num_Samples >= 2 & n_over27PTL >=1 | num_Samples >= 2 & n_22to27PTL >=1,"Cat5",
                         ifelse(num_Samples == 1 & n_22to27PTL >=1,"Cat3B",
                                ifelse(n_8to21PTL >=1,"Cat3C",
                                       ifelse(n_less7PTL >=1,"Cat2","")))))

bio_E <- BioCriteria %>% 
  filter(Qualifier == "DQL=E") %>%
  group_by(AU_ID) %>%
  summarise(num_Samples = n()) %>%
  mutate(IR_Cat = "Cat3")       


write.csv(MWCF_AU_sum,"MWCF_AU_sum.csv")
write.csv(WC_AU_sum,"WC_AU_sum.csv")
write.csv(bio_E,"Cat3_AU_sum.csv")
write.csv(BioCriteria,"Bio_Data.csv")
