biodata <- biodata %>% 
  filter(Qualifier == "DQL=A") %>%
  filter(ID == "% Taxa Loss")

MWCF_AU_sum = biodata %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST") %>%
  group_by(AU_ID.y) %>%
  summarise(n = "n()",
            n_TLOver15P = sum(IndexTypeID >= 20),  ## Cat 5 n= 1 or more  
            n_TL20to15P = sum(IndexTypeID > 15 & IndexTypeID < 20), # cat 5 if n is greater than or equal to 2, if n=1 cat 3B
            n_TL14to9P = sum(IndexTypeID > 9 & IndexTypeID < 14), # cat 3D
            N_TLless8P = sum(IndexTypeID <= 8)) # cat 2 
