library(tidyverse)
library(openxlsx)


categories <- read.xlsx("ATTAINS/Rollup/Basin_categories/ALL BASINS_categories.xlsx")

delistings <- read.csv("ATTAINS/Rollup/Basin_categories/ALL BASINS_delistings.csv",
                       stringsAsFactors = FALSE) %>%
  select(AU_ID,
         Char_Name,
         Pollu_ID,
         WQstd_code,
         Period,
         Delist) %>%
  mutate_all(as.character) %>%
  mutate(Period = ifelse(Period == "", NA, Period ),
         Delist = str_trim(Delist)) 


# categories_summary <- categories %>%
#   left_join(delistings,by = c("AU_ID", "Char_Name", "Pollu_ID", "WQstd_code", "Period")) %>%
#   group_by(Char_Name) %>%
#   summarise(num_impairments = n_distinct(AU_ID[IR_category ==  "Category 5" | IR_category ==  "Category 4A"]),
#             num_previous_listings = n_distinct(AU_ID[previous_IR_category == "Category 5"]),
#             num_delist = n_distinct(AU_ID[Delist == "YES"]),
#             percent_increase = round((num_impairments - (num_previous_listings -num_delist))/(num_previous_listings - num_delist) * 100, 2)) %>%
#   arrange(desc(num_impairments))
  

categories_summary <- categories %>%
  left_join(delistings,by = c("AU_ID", "Char_Name", "Pollu_ID", "WQstd_code", "Period")) %>%
  mutate(Impaired = ifelse(IR_category ==  "Category 5" | IR_category ==  "Category 4A", 1, 0 ),
         previous_impared = case_when(previous_IR_category == "Category 5" ~ 1, 
                                      TRUE ~ 0),
         delist = case_when(Delist == "YES" ~ 1,
                            TRUE ~ 0)) %>%
  group_by(Char_Name) %>%
  summarise(Assessed_AUs = n_distinct(AU_ID),
            Impaired_AUs = n_distinct(AU_ID[Impaired == 1]),
            AUs_previous_impared = n_distinct(AU_ID[previous_impared == 1]),
            delist =  n_distinct(AU_ID[delist == 1]),
            percent_impared = Impaired_AUs/Assessed_AUs * 100,
            percent_increase = (Impaired_AUs - (AUs_previous_impared - delist))/(AUs_previous_impared - delist) * 100
            ) %>%
  arrange(desc(Impaired_AUs))


for_jennfier <- categories_summary <- categories %>%
  mutate(Char_Name = case_when(Char_Name == "HARMFUL ALGAL BLOOMS" ~ "Harmful Algal Blooms",
                               TRUE ~ Char_Name)) %>%
  left_join(delistings,by = c("AU_ID", "Char_Name", "Pollu_ID", "WQstd_code", "Period")) %>%
  mutate(Impaired = ifelse(IR_category ==  "Category 5" | IR_category ==  "Category 4A", 1, 0 ),
         previous_impared = case_when(previous_IR_category == "Category 5" ~ 1, 
                                      TRUE ~ 0),
         delist = case_when(Delist == "YES" ~ 1,
                            TRUE ~ 0)) %>%
  group_by(Char_Name) %>%
  summarise(Impaired_AUs = n_distinct(AU_ID[Impaired == 1]),
            AUs_previous_impared = n_distinct(AU_ID[previous_impared == 1]),
            delist =  n_distinct(AU_ID[delist == 1]),
            percent_increase = (Impaired_AUs - (AUs_previous_impared - delist))/(AUs_previous_impared - delist) * 100
  ) %>%
  arrange(desc(Impaired_AUs)) %>%
  filter(Char_Name %in% c("Harmful Algal Blooms",
                          "HARMFUL ALGAL BLOOMS",
                          'Biocriteria',
                          'Excess Algal Growth',
                          'Aquatic Weeds',
                          'NOXIOUS AQUATIC PLANTS'))
  


data = head(categories_summary, n = 4)