library(lubridate)




temp_analysis <- Results_censored_temp %>%
  mutate(Crit_period_start = mdy(paste0("7/1/",year(ActStartD))),
         Cirt_period_end = mdy(paste0("9/30/",year(ActStartD))),
         Start_spawn = ifelse(!is.na(SpawnStart), paste0(SpawnStart,"/",year(ActStartD)), NA ) ,
         End_spawn = ifelse(!is.na(SpawnEnd), paste0(SpawnEnd,"/",year(ActStartD)), NA ),
         Start_spawn = mdy(Start_spawn),
         End_spawn = mdy(End_spawn),
         End_spawn = if_else(End_spawn < Start_spawn, End_spawn + years(1), End_spawn ),
         ActStartD = ymd(ActStartD), 
         In_crit_period = ifelse(between(ActStartD,Crit_period_start, Cirt_period_end), 1, 0 ),
         Spawn_type = ifelse((ActStartD >= Start_spawn & ActStartD <= End_spawn & !is.na(Start_spawn)),  "Spawn", "Not_Spawn"),
         Violation = ifelse(Spawn_type == "Spawn" & Result_cen > 13, 1,
                            ifelse(Spawn_type == "Not_Spawn" & Result_cen > Temp_Criteria, 1, 0)),
         Spawn_Violation = ifelse(Spawn_type == "Spawn" & Violation == 1, 1, 0 )
         ) %>%
  filter(AU_ID == "OR_WS_170702010206_05_102026") %>%
  arrange(ActStartD)



#Need to finish critical period checker. How do we bring this in?
Crit_periods <- temp_analysis %>%
  mutate(year = year(ActStartD)) %>%
  group_by(AU_ID, year, Spawn_type) %>%
  summarise(crit_period_counts = sum(In_crit_period)) 
  

# # Break data into three year chunks to flag out definite category  --------
# 
# # period 1 - 1/1/2008 - 12/31/2010
# 
# period1 <- temp_analysis %>%
#   filter(ActStartD >= mdy("1/1/2008") & ActStartD < mdy("1/1/2011")) %>%
#   group_by(AU_ID) %>%
#   summarise(per1.sumviolation = sum(Violation) ,
#          per1.MLoc_count = n_distinct(MLocID))
# 
# # period 2 - 1/1/2011 - 12/31/13
# period2 <- temp_analysis %>%
#   filter(ActStartD >= mdy("1/1/2011") & ActStartD < mdy("1/1/2014")) %>%
#   group_by(AU_ID) %>%
#   summarise(per2.sumviolation = sum(Violation) ,
#          per2.MLoc_count = n_distinct(MLocID)) 
# 
# #period 3 - 1/1/2014 - 12/31/2016
# period3 <- temp_analysis %>%
#   filter(ActStartD >= mdy("1/1/2014") & ActStartD < mdy("1/1/2016")) %>%
#   group_by(AU_ID) %>%
#   summarise(per3.sumviolation = sum(Violation) ,
#             per3.MLoc_count = n_distinct(MLocID)) 
# 
# 
# #period 4 - 1/1/2017 - 1/1/2018
# period4 <- temp_analysis %>%
#   filter(ActStartD >= mdy("1/1/2016") & ActStartD < mdy("1/1/2018")) %>%
#   group_by(AU_ID) %>%
#   summarise(per4.sumviolation = sum(Violation) ,
#             per4.MLoc_count = n_distinct(MLocID)) 
# 
# #period 5 - 1/1/2018 - 7/1/2018
# period5 <- temp_analysis %>%
#   filter(ActStartD >= mdy("1/1/2018") & ActStartD < mdy("7/1/2018")) %>%
#   group_by(AU_ID) %>%
#   summarise(per5.sumviolation = sum(Violation) ,
#             per5.MLoc_count = n_distinct(MLocID)) 
# 
# 
# 
# # join all the time periods together
# # definite cat 5's are sites with 2 or more violations in calendar year
# # Need to review for site that do not have 2 or more in calendar year, but 
# # may have 2 or more in rolling year
# # Sites flagged as More Review will need more review, obviously
# temp_summary <- Results_censored_temp %>%
#   left_join(period1, by = "AU_ID") %>%
#   left_join(period2, by = "AU_ID") %>%
#   left_join(period3, by = "AU_ID") %>%
#   left_join(period4, by = "AU_ID") %>%
#   left_join(period5, by = "AU_ID") %>%
#   mutate(Category = ifelse(per1.sumviolation >= 2 & !is.na(per1.sumviolation) |
#                               per2.sumviolation >= 2 & !is.na(per2.sumviolation) |
#                               per3.sumviolation >= 2 & !is.na(per3.sumviolation) |
#                               per4.sumviolation >= 2 & !is.na(per4.sumviolation) |
#                               per5.sumviolation >= 2 & !is.na(per5.sumviolation),  "Cat5", 
#                               "More_Review" ))
# 
# #Create table of definite Category 5's
# temp_cat5 <- temp_summary %>%
#   group_by(AU_ID) %>%
#   summarise(IR_Cat = first(Category)) %>%
#   filter(IR_Cat == "Cat5")
# 
# 
# 
# ReviewAUs <- temp_summary %>%
#   filter(Category == "More_Review")
# 
# # These will need more review
# Review <- temp_analysis %>%
#   filter(AU_ID %in% unique(ReviewAUs$AU_ID))
# 
# 
# Au_review_list <- list()
# 
# for (i in 1:length(unique(Review$AU_ID))){
#   
#   AU_4review <- unique(Review$AU_ID)[i]
#   
#   Review_AU <- Review %>%
#     filter(AU_ID == AU_4review) 
#   
#   for(j in 1:nrow(Review_AU)){
#     
#     start3yr <- Review_AU$ActStartD[j] - years(3)
#     end3yr <- Review_AU$ActStartD[j]
#     
#     period3yr <- Review_AU %>%
#       filter(ActStartD >= start3yr & ActStartD <= end3yr)
#     
#     num_violations = sum(period3yr$Violation)
#     Review_AU[j,"Violations_3yr"] <- num_violations
#     
#   }
#   
#   Au_review_list[[i]] <- Review_AU
#   
# }
# 
# reviewed_data <- bind_rows(Au_review_list) %>%
#   group_by(AU_ID) %>%
#   summarise(maxviolation = max(Violations_3yr))
# 
# 


#experimental

Au_review_list <- list()


pb <- txtProgressBar(min = 0, max = length(unique(temp_analysis$AU_ID)), style = 3)

for (i in 1:length(unique(temp_analysis$AU_ID))){
  
  AU_4review <- unique(temp_analysis$AU_ID)[i]
  
  print(paste("Beginning AU:", AU_4review, "-", i, "of", length(unique(temp_analysis$AU_ID)) ))
  
  Review_AU <- temp_analysis %>%
    filter(AU_ID == AU_4review) 
  
  pb <- txtProgressBar(min = 0, max = nrow(Review_AU), style = 3)
  for(j in 1:nrow(Review_AU)){
    
    start3yr <- Review_AU$ActStartD[j] - years(3)
    end3yr <- Review_AU$ActStartD[j]
    
    period3yr <- Review_AU %>%
      filter(between(ActStartD,start3yr, end3yr ))
    
    num_violations = sum(period3yr$Violation)
    samples_crit_period <- sum(period3yr$In_crit_period)
    samples_spawn <- nrow(subset(period3yr, Spawn_type == "Spawn"))
    spawn_violations <- sum(period3yr$Spawn_type == "Spawn" & period3yr$Violation == 1)
    
    
    Review_AU[j,"Violations_3yr"] <- num_violations
    Review_AU[j,"Violations_in_Spawning"] <- spawn_violations
    Review_AU[j,"Samples_in_crit_period"] <- samples_crit_period
    Review_AU[j,"samples_in_spawn_period"] <- samples_spawn
    
    setTxtProgressBar(pb, j)
    
  }
  
  Au_review_list[[i]] <- Review_AU
 
  close(pb)
}



reviewed_data <- bind_rows(Au_review_list) %>%
  arrange(AU_ID, ActStartD)
# 
# Temp_IR_categories <- reviewed_data %>%
#   group_by(AU_ID) %>%
#   mutate(total_violations = sum(Violation),
#          Spawn_Violation_count = sum(Spawn_Violation) ) %>%
#   arrange(ActStartD) %>%
#   filter(Violations_3yr %in% range(Violations_3yr))
#   slice(which.max(Violations_3yr)) %>%
#   mutate(IR_category = ifelse(Violations_3yr >= 2, "Cat5", 
#                               ifelse(Violations_3yr < 2 & 
#                                        Samples_in_crit_period == 0 &
#                                        samples_in_spawn_period == 0, "Cat3", 
#                                      ifelse(Violations_3yr == 1, "Cat3B", 
#                                             "Cat2")))) %>%
#   select(AU_ID,IR_category, total_violations, Spawn_Violation_count)
# # 
#   
  
  
  Temp_IR_categories <- reviewed_data %>%
    group_by(AU_ID) %>%
    mutate(total_violations = sum(Violation),
           Spawn_Violation_count = sum(Spawn_Violation) ) %>%
    arrange(ActStartD) %>%
    filter(Violations_3yr %in% range(Violations_3yr)) %>%
    summarise(Violations_3yr = max(Violations_3yr),
              total_violations = max(total_violations),
              Spawn_Violation_count = max(Spawn_Violation_count),
              max_3yr_Samples_in_crit_period = max(Samples_in_crit_period),
              max_3yr_samples_in_spawn_period = max(samples_in_spawn_period)) %>%
    mutate(IR_category = ifelse(Violations_3yr >= 2, "Cat5", 
                                ifelse(Violations_3yr < 2 & 
                                         max_3yr_Samples_in_crit_period == 0 &
                                         max_3yr_samples_in_spawn_period == 0, "Cat3", 
                                       ifelse(Violations_3yr == 1, "Cat3B", 
                                              "Cat2")))) %>%
    select(AU_ID,IR_category, total_violations, Spawn_Violation_count, max_3yr_Samples_in_crit_period,
           max_3yr_samples_in_spawn_period)
  # 
  # 
  
  
# 
# temp_category3b <- reviewed_data %>%
#   filter(Samples_in_crit_period == 0 & samples_in_spawn_period == 0) %>%
#   distinct(AU_ID) %>%
#   mutate(IR_category = "Cat3b")
# 
# temp_assessment_data <- reviewed_data %>%
#   filter(Samples_in_crit_period != 0 | samples_in_spawn_period != 0)  %>%
#   #group_by(AU_ID) %>%
#   #summarise(max_violation_3yr = max(Violations_3yr)) %>%
#   filter(AU_ID == 'OR_WS_170702040304_05_102199')
# 
# 
# temp_assessment_data <- reviewed_data %>%
#   filter(!(AU_ID %in% temp_category3b$AU_ID)) %>%
#   group_by(AU_ID) %>%
#   summarise(max_violation_3yr = max(Violations_3yr))
# 
# 
# temp_category3b <- reviewed_data %>%
#   filter(Samples_in_crit_period == 0 & samples_in_spawn_period == 0) %>%
#   distinct(AU_ID) %>%
#   mutate(IR_category = "Cat3b")
# 
# unique(temp_category3b$AU_ID)
