library(tidyverse)
library(openxlsx)

old_categories <- read.xlsx('Other tools/corrections_checker/data/ALL BASINS_categories_pre_temp_fix.xlsx')

new_categories <- read.xlsx('Other tools/corrections_checker/data/ALL BASINS_categories_temp_fix.xlsx')



old_categories_diff <- old_categories %>%
  mutate(Period = case_when(Period == 'Year round' ~ 'Year Round', 
                            Period == 'Spawning' ~ 'Spawn',
                            TRUE ~ Period)) %>%
  rename(original_assessment_catgory = IR_category) %>%
  select(AU_ID, Pollu_ID, WQstd_code, Period, original_assessment_catgory )
  


Differences <- new_categories %>%
  select(AU_ID, Pollu_ID, WQstd_code, Char_Name, Period, IR_category ) %>%
  full_join(old_categories_diff) %>%
  filter(original_assessment_catgory != IR_category) %>%
  filter(Char_Name != 'Chlordane')


write.xlsx(Differences, file = 'Other tools/corrections_checker/data/differences.xlsx')
