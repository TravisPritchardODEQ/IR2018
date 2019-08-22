library(tidyverse)
library(openxlsx)


AU_names <- read.csv("ATTAINS/AU_names.csv", stringsAsFactors = FALSE) %>%
  select(AU_ID, AU_Name)




coastal_shellfish_listings <- AU_names %>%
  filter(grepl("CL", AU_ID)) %>%
  mutate(Char_Name  = "Shellfish Toxins",
         Pollu_ID = NA,
         WQstd_code = 8,
         Data_File = 'HH Toxics',
         Period = NA,
         IR_category = 'Category 5',
         analysis_comment_2018 = 'Based on fish or shellfish consumption advisories issued by ODA or OHA',
         Data_Review_Code = "",
         Data_Review_Comment = "",
         Rational = "",
         year_assessed = '2018',
         Year_listed = '2018',
         previous_IR_category = NA,
         Assessed_in_2018 = "YES",
         assessment_result_2018 = 'Category 5',
         Action_ID = NA,
         TMDL_Name = NA,
         Review_Comment = NA,
         Revised_Category =NA
         )

write.xlsx(coastal_shellfish_listings, paste0("ATTAINS/Rollup/Basin_categories/", "shellfish_categories.xlsx"),
           row.names = FALSE,
           na = "")
