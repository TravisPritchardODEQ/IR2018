library(tidyverse)
library(openxlsx)
# Load assessment result data
all_bains_categories <- read.xlsx("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Rollup/ALL BASINS_categories.xlsx")


list_303d <- all_bains_categories %>%
  filter(IR_category == "Category 5") %>%
  mutate(Rationale = ifelse(IR_category ==  "Category 5" |  IR_category == "Category 4A" | 
                              IR_category == "Category 4"  | 
                              IR_category == "Category 4b" |  
                              IR_category == "Category 4C", Rationale, '' )) %>%
  mutate(Rationale = ifelse(is.na(Rationale), '', Rationale )) %>%
  mutate(Rationale = ifelse(Assessed_in_2018 == 'NO', "Carried forward from previous listing", Rationale )) %>%
  mutate(Rationale = ifelse(Rationale == '', "Carried forward from previous listing", Rationale )) %>%
  select(-Data_Review_Comment, -analysis_comment_2018, -Data_Review_Code, 
         -Action_ID, -TMDL_Name, -Review_Comment, -Revised_Category)


write.xlsx(list_303d, "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Rollup/303d_list.xlsx")
