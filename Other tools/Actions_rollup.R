
library(tidyverse)
library(openxlsx)

# Load assessment result data
all_bains_categories <- read.xlsx("E:/Documents/IR2018/ATTAINS/Rollup/Basin_categories/ALL BASINS_categories.xlsx")

TMDL_prioprties <- read.xlsx("C:/Users/tpritch/Downloads/Copy of 2018-2020_Cat4-5_AU_Priority_20200401.xlsx")


priority_4as <- TMDL_prioprties %>%
  filter(IR_category == "Category 4A") %>%
  select(AU_ID, Char_Name, Period, IR_category,TMDL_Project_Name ) %>%
  rename(TMDL_category = IR_category)

joined <- all_bains_categories %>%
  select(AU_ID, Char_Name, Period, IR_category) %>%
  left_join(priority_4as) %>%
  filter(!is.na(TMDL_category)) 


incorrect_category <- joined %>%
  filter(IR_category == TMDL_category)

correct_category <- joined %>%
  filter(IR_category != TMDL_category)

unique(correct_category$TMDL_Project_Name)


whole_TMDLS <-setdiff(unique(incorrect_category$TMDL_Project_Name), unique(correct_category$TMDL_Project_Name))

one_off_misisng <- incorrect_category %>%
  filter(!TMDL_Project_Name %in% c("Lower Willamette, Clackamas, and Sandy Subbasins", "Walla Walla Subbasin"))

write.xlsx(incorrect_category, file = "Incorrect_4a_Assignments.xlsx")

#Load actions

actions <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/actions.csv",
                    stringsAsFactors = FALSE) %>%
  select(ACTION_ID, ACTION_NAME)

AU_Actions <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/AU_Action.csv",
                       stringsAsFactors = FALSE)%>%
  select(AU_ID, Action_ID) %>%
  mutate(Action_ID = str_replace_all(Action_ID, "[\r\n]" , ""))

Action_Parameter <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/Action_Parameter.csv",
         stringsAsFactors = FALSE) %>%
  select(ACTION_ID, Pollu_ID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID))

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")


Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right")) %>%
  select(Pollu_ID,Pollutant_DEQ.WQS )


Action_paramter_Pollutants <- Action_Parameter %>%
  left_join(Pollutants)

AU_Action_put_together <- AU_Actions %>%
  rename("ACTION_ID" = "Action_ID") %>%
  left_join(actions) %>%
  left_join(Action_paramter_Pollutants)


Unique_actions <- AU_Action_put_together %>%
  select(ACTION_ID, ACTION_NAME, Pollu_ID, Pollutant_DEQ.WQS) %>%
  distinct()

incorrect_TMDL <- incorrect_category %>%
  select(AU_ID, TMDL_Project_Name)

incorrect_AU_Action_put_together <- AU_Action_put_together %>%
  filter(AU_ID %in% unique(incorrect_category$AU_ID)) %>%
  arrange(AU_ID) %>%
  left_join(incorrect_TMDL)



write.xlsx(incorrect_AU_Action_put_together, file = "Incorrect_4a_Assignments_ACTIONS.xlsx")




# Action AU Parameter combine ----------------------------------------------------------
AU_Actions <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/older action files/AU_Action.csv",
                       stringsAsFactors = FALSE)%>%
  select(AU_ID, Action_ID) %>%
  mutate(Action_ID = str_replace_all(Action_ID, "[\r\n]" , "")) %>%
  rename(ACTION_ID = Action_ID)



Action_Parameter <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/older action files/Action_Parameter.csv",
                             stringsAsFactors = FALSE) %>%
  select(ACTION_ID, Pollu_ID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID))

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")


Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right")) %>%
  select(Pollu_ID,Pollutant_DEQ.WQS )

Action_AU_Parameter <- Action_Parameter %>%
  left_join(Pollutants,by = "Pollu_ID") %>%
  right_join(AU_Actions) %>%
  select(ACTION_ID, AU_ID, Pollu_ID, Pollutant_DEQ.WQS)

write.csv(Action_AU_Parameter, file="//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/Action_AU_Parameter.csv",
          row.names = FALSE)


# associated_actions ------------------------------------------------------

  AU_Actions <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/older action files/AU_Action.csv",
                         stringsAsFactors = FALSE)%>%
  select(AU_ID, Action_ID) %>%
  mutate(Action_ID = str_replace_all(Action_ID, "[\r\n]" , "")) %>%
  rename(ACTION_ID = Action_ID)



Action_Parameter <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/older action files/Action_Parameter.csv",
                             stringsAsFactors = FALSE) %>%
  select(ACTION_ID, Pollu_ID) %>%
  mutate(Pollu_ID = as.character(Pollu_ID))

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")


Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right")) %>%
  select(Pollu_ID,Attains_PolluName )



actions <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/actions.csv",
                    stringsAsFactors = FALSE) %>%
  select(ACTION_ID, ACTION_NAME, ACTION_TYPE)


associated_actions <- Action_Parameter %>%
  left_join(Pollutants,by = "Pollu_ID") %>%
  right_join(AU_Actions) %>%
  left_join(actions) %>%
  select(AU_ID, Attains_PolluName,ACTION_ID,  ACTION_TYPE)

write.xlsx(associated_actions, file = "associated_actions.xlsx")



