library(tidyverse)
library(openxlsx)
library(viridis)
library(RColorBrewer)

options(scipen = 999999)

IR_categories <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_categories.xlsx")

AU_layer <- read.xlsx('Other tools/statistics generator/AU layer.xlsx')


IR_category_factor <- factor(IR_categories$IR_category, levels = c('Unassigned',
                                                                     "-",
                                                                     "Category 3C",
                                                                     "Category 3D",
                                                                     "Category 3",
                                                                     "Category 3B",
                                                                     "Category 2",
                                                                     "Category 4",
                                                                     "Category 4B",
                                                                     "Category 4C",
                                                                     "Category 4A",
                                                                     "Category 5"),
                             ordered = TRUE)

IR_categories$IR_category <- IR_category_factor


total_listings <- IR_categories %>%
  filter(IR_category %in% c("Category 4",
                            "Category 4B",
                            "Category 4C",
                            "Category 4A",
                            "Category 5"),
         Year_listed == "2018") %>%
  group_by(Char_Name) %>%
  summarise(Number_new_impaired_listings = n()) %>%
  arrange(desc(Number_new_impaired_listings))


         Assessed_in_2018 == "",
         Char_Name == "Temperature")



total_state_rivermiles <- sum(AU_layer$AU_LenMiles)

stats_2018 <- IR_categories %>%
  group_by(AU_ID) %>%
  mutate(group_cat = max(IR_category),
         match = ifelse(IR_category == group_cat, 1, 0 )) %>%
  filter(match == 1) %>%
  filter(row_number() == 1) %>%
  left_join(select(AU_layer, AU_ID, AU_LenMiles )) %>%
  #filter(IR_category == 'Category 5' | IR_category == 'Category 4A') %>%
  group_by(Char_Name, Pollu_ID) %>%
  summarise(number_assessments = n(),
            number_listing = sum(group_cat =='Category 5' | group_cat == 'Category 4A' ),
            number_miles_assessed = round(sum(AU_LenMiles, na.rm = TRUE),0),
            number_listed_miles = round(sum(AU_LenMiles[group_cat =='Category 5' |group_cat == 'Category 4A' ], na.rm = TRUE),0)) %>%
  arrange(desc(number_listing)) 



stats_2018b <- IR_categories %>%
  group_by(AU_ID) %>%
  mutate(group_cat = max(IR_category),
         match = ifelse(IR_category == group_cat, 1, 0 )) %>%
  filter(match == 1) %>%
  filter(row_number() == 1) %>%
  left_join(select(AU_layer, AU_ID, AU_LenMiles ))

Impaired_rivermiles_2018 <-sum(stats_2018b$AU_LenMiles, na.rm = TRUE)

percent_Impaired_rivermiles_2018 <- Impaired_rivermiles_2018 / total_state_rivermiles

number_impairements_2018 <- nrow(stats_2018b)

nrow(stats_2018b)/6875



crosswalked_2012_listings <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Crosswalk_2012List/ATTAINS_uploads/ATTAINS_download/2012Crosswalk_Final.csv",
                                      stringsAsFactors = FALSE) 

stats_2012 <- crosswalked_2012_listings %>%
  distinct(ASSESSMENT_UNIT_ID,Pollu_ID, WQstrd_code, .keep_all = TRUE) %>%
  rename(AU_ID = ASSESSMENT_UNIT_ID,
         Char_Name = PARAM_NAME) %>%
  left_join(select(AU_layer, AU_ID, AU_LenMiles )) %>% 
  group_by(Char_Name, Pollu_ID) %>%
  summarise(number_listing_2012 = n(),
            number_listed_miles_2012 = round(sum(AU_LenMiles),0)) %>%
  arrange(desc(number_listing_2012)) %>%
  ungroup() %>%
  mutate(Pollu_ID = as.character(Pollu_ID))

stats_2012b <- crosswalked_2012_listings %>%
  distinct(ASSESSMENT_UNIT_ID,Pollu_ID, WQstrd_code, .keep_all = TRUE) %>%
  rename(AU_ID = ASSESSMENT_UNIT_ID,
         Char_Name = PARAM_NAME) %>%
  left_join(select(AU_layer, AU_ID, AU_LenMiles )) %>%
  group_by(AU_ID) %>%
  filter(row_number() == 1)

Impaired_rivermiles_2012 <- sum(stats_2012b$AU_LenMiles, na.rm = TRUE)
  nrow(stats_2012b) / 6874
  
  percent_Impaired_rivermiles_2012 <- Impaired_rivermiles_2012 / total_state_rivermiles

FAQ_stats <- stats_2018 %>%
  left_join(select(stats_2012, Pollu_ID, number_listing_2012, number_listed_miles_2012)) 

FAQ_stats <- FAQ_stats[,c(1,3,5,4,6,7,8)]

write.csv(FAQ_stats, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Communications/Talking Points/2018_IR_listing_statistics.csv",
          row.names = FALSE)


Assessed_AU_s <- AU_layer %>%
  filter(AU_ID %in% IR_categories$AU_ID)

sum(Assessed_AU_s$AU_LenMiles)



stats_for_EQC <- stats_2018 %>%
  mutate(Percent_AUs_assessed = round(number_assessments / 6845 * 100, 1),
         Percent_of_assessed_impaired = round(number_listing / number_assessments * 100, 1)) %>%
  filter(Char_Name %in% c('Temperature', 'Dissolved Oxygen',
                          'E. coli', 'pH'
  )) %>%
  mutate(Percent_of_state_AU_impaired = round(number_listing / 6845 * 100, 1)) %>%
  ungroup() %>%
  select(Char_Name, Percent_AUs_assessed, Percent_of_assessed_impaired,Percent_of_state_AU_impaired)

#write.csv(stats_for_EQC, file = "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Communications/Talking Points/2018_IR_listing_EQC_statistics.csv",
#          row.names = FALSE)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(stats_for_EQC, aes(x = Char_Name, y = Percent_of_state_AU_impaired, fill = Char_Name)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_classic()+
  labs(title = "Statewide Impairements", x = NULL, 
    y = "Percent of assessment units impaired - statewide") +
  scale_fill_manual(values = cbp1)+
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))
  

stats_2018_by_type <- IR_categories %>%
  group_by(AU_ID, Pollu_ID) %>%
  mutate(group_cat = max(IR_category),
         match = ifelse(IR_category == group_cat, 1, 0 )) %>%
  filter(match == 1) %>%
  filter(row_number() == 1) %>%
  left_join(select(AU_layer, AU_ID, AU_LenMiles )) %>%
  ungroup() %>%
  mutate(Char_Name = ifelse(Char_Name ==	'Dissolved Oxygen- Cold Water' |Char_Name ==	'Dissolved Oxygen- Cool Water'  , 'Dissolved Oxygen', 
                            ifelse(Char_Name ==	'Enterococci', 'Enterococcus', Char_Name ))) %>%
  mutate(Type = case_when(grepl("CL", AU_ID) ~ "Coastline",
                          grepl("LK", AU_ID) ~ "Lake / Reservoir",
                          grepl("SR", AU_ID) ~ "River / Stream",
                          grepl("WS", AU_ID) ~ "Watershed Unit",
                          grepl("EB", AU_ID) ~ "Estuary / Bay",
                          TRUE ~ "Lake / Reservoir")) %>%
  #filter(IR_category == 'Category 5' | IR_category == 'Category 4A') %>%
  group_by(Char_Name, Pollu_ID, Type) %>%
  summarise(number_assessments = n(),
            number_listing = sum(group_cat =='Category 5' | group_cat == 'Category 4A' ),
            number_miles_assessed = round(sum(AU_LenMiles, na.rm = TRUE),0),
            number_listed_miles = round(sum(AU_LenMiles[group_cat =='Category 5' |group_cat == 'Category 4A' ], na.rm = TRUE),0)) %>%
  arrange(desc(number_listing)) %>%
  mutate(Percent_impaired_of_assessed = round(number_listing / number_assessments * 100, 1)) %>%
  select(Char_Name, Type, Percent_impaired_of_assessed) %>%
  spread(Type, Percent_impaired_of_assessed, fill = "-") %>%
  arrange(Char_Name) %>%
  filter(Char_Name %in% c('Temperature', 'Dissolved Oxygen',
                          'E. coli', 'pH'
                          ))

  

# Stats for Factsheet -----------------------------------------------------

impaired_1_or_more <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_Impaired_1orMoreUses.csv",
                               stringsAsFactors = FALSE)

Fact_stats <- impaired_1_or_more %>%
  mutate(status = case_when(Impaired_Uses == "-" & attaining_uses == "-" ~ 'Insufficient data',
                            Impairment_cause == "." ~ "Attaining",
                            Impaired_Uses != "-" ~ "Impaired",
                            TRUE ~ "ERROR")) %>%
  group_by(status) %>%
  summarise(number = n(),
            Percent_of_assessed = number / 2858 * 100,
            Percent_of_state = number / 6845 * 100)



# BU chart ----------------------------------------------------------------

BU_counts <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_BU_counts.csv",
                      stringsAsFactors = FALSE)


BU_stats <- BU_counts %>%
  filter(ben_use != "" ) %>%
  mutate(status = case_when(Category.5 != "-" | Category.4A != "-" ~ "Impaired",
                            Category.2 != "-" ~ "Attains",
                            Category.3B != "-" |
                              Category.3C != "-" |
                              Category.3D != "-" ~ "Insuffcient Data",
                            TRUE ~ "Other")
       ) %>%
  mutate(Impaired = ifelse(status == "Impaired", 1, 0 ),
         Attains = ifelse(status == "Attains", 1, 0),
         Insuffcient_data = ifelse(status == "Insuffcient Data", 1, 0 )) %>%
  group_by(ben_use) %>%
  summarise(number_impaired = sum(Impaired),
            number_attain = sum(Attains),
            number_insuffcient = sum(Insuffcient_data)) %>%
  gather(key = "Status", value = "Count", 2:4) %>%
  filter(Status == 'number_impaired') %>%
  mutate(ben_use = ifelse(ben_use == 'Fishing' , 'Fishing (Consumption)', ben_use ))
  
  
ggplot(data =BU_stats, aes(x = ben_use, y = Count, fill = Status ) ) +
  geom_bar(stat="identity", position=position_dodge()) + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(angle = -20))
  

pie_chart

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    axis.text = element_blank()
  )

cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



ggplot(BU_stats, aes(x = "", y = Count, fill = ben_use)) +
  geom_bar(width = 1, stat = "identity", position="dodge") +
  #coord_polar("y", start=0) +
  theme_classic() +
  theme(axis.text.x=element_blank()) + 
  scale_fill_manual(values = cbp2) +
 # scale_y_discrete(expand = c(0, 0)) +
  #blank_theme + 
  theme(legend.position = "bottom", legend.direction = "horizontal") +labs(title = "Beneficial Use Impairements", 
    fill = NULL)+labs(subtitle = "Number of Assessment Units impaired for each beneficial use") + theme(axis.ticks = element_line(linetype = "blank"))+labs(x = NULL, y = NULL)



# Delisting stats ---------------------------------------------------------


delistings_all <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_delistings.csv",
                           stringsAsFactors = FALSE)


delistings_stats <- delistings_all %>%
  group_by(Char_Name, WQstd_code, Pollu_ID) %>%
  summarise(num_delistings = n()) %>%
  ungroup() %>%
  mutate(WQstd_code = case_when(WQstd_code == 15 ~ "Aquatic Life",
                          WQstd_code == 16 ~ "Human Health",
                          TRUE ~ "")) %>%
  rename(Type = WQstd_code) %>%
  arrange(desc(num_delistings)) %>%
  select(-Pollu_ID)


write.csv(delistings_stats, file = "Other tools/statistics generator/delist_statistics.csv",
          row.names = FALSE)



# Road show stats ---------------------------------------------------------

IR_categories <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_categories.xlsx")

AU_layer <- read.xlsx('Other tools/statistics generator/AU layer.xlsx')


IR_category_factor <- factor(IR_categories$IR_category, levels = c('Unassigned',
                                                                   "-",
                                                                   "Category 3C",
                                                                   "Category 3D",
                                                                   "Category 3",
                                                                   "Category 3B",
                                                                   "Category 2",
                                                                   "Category 4",
                                                                   "Category 4B",
                                                                   "Category 4C",
                                                                   "Category 4A",
                                                                   "Category 5"),
                             ordered = TRUE)

IR_categories$IR_category <- IR_category_factor

IR_categories_by_AU <- IR_categories %>%
  filter(!IR_category %in% c('Unassigned',
                            "-") ) %>%
  group_by(AU_ID) %>%
  summarise(max_cat = max(IR_category)) %>%
  ungroup() %>%
  left_join(AU_layer) %>%
  mutate(adjusted_cat = case_when(max_cat %in% c("Category 4B","Category 4C","Category 4A") ~ "Category 4",
                                  max_cat %in% c("Category 3C","Category 3D","Category 3B") ~ "Category 3",
                                  TRUE ~ as.character(max_cat))) %>%
  group_by(adjusted_cat) %>%
  summarise(Total_River_miles = sum(AU_LenMiles, na.rm = TRUE),
            num_AUs = n())


write.csv(IR_categories_by_AU, file = "Other tools/statistics generator/stream_status_stats - of assessed AUs.csv",
          row.names = FALSE)





BU_summary <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Draft List/Rollup/Basin_categories/ALL BASINS_BU_summary.csv",
                       stringsAsFactors = FALSE)

BU_stats <- BU_summary %>%
  mutate(support_level = case_when(Assessed_condition == "Use not assessed" ~ "Unassessed",
                                   Assessed_condition %in% c("Not supported", 
                                                             "Not supported. TMDL in place", 
                                                             " Data indicate that at least one designated use is not supported, but a TMDL is not needed to address the pollutant",
                                                             "Impairment is caused by pollution, not a pollutant")  ~ "Not_supported",
                                   Assessed_condition == "Standards met for all assessed parameters" ~ "Supported",
                                   Assessed_condition %in% c("Insufficient data to determine use support",
                                                             "Insufficient data to determine use support because numeric criteria are less than quantification limits",
                                                             "Insufficient data to determine use support, but some data indicate non-attainment of a criterion",
                                                             "Insufficient data to determine use support, but data indicated marginal bilogical condition") ~ "Insufficient",
                                   TRUE ~ "ERROR"
                                   
                                    )
         ) %>%
  group_by(ben_use) %>%
  summarise(support_percent = round(sum(support_level == "Supported")/n() * 100, 1),
            Nonsupport_percent =  round(sum(support_level == "Not_supported")/n() * 100, 1),
            Insufficient_percent =  round(sum(support_level == "Insufficient")/n() * 100, 1),
            Unassessed_percet = round(sum(support_level == "Unassessed")/n() * 100, 1)
            
  )

write.csv(BU_stats, file = "Other tools/statistics generator/Ben_use_stats-of assessed AUs.csv",
          row.names = FALSE)

