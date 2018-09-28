library(lubridate)



temp_analysis <- Results_censored %>%
  mutate(Start_spawn = mdy(paste0(SpawnStart,"/",year(ActStartD))),
         End_Spawn = mdy(paste0(SpawnEnd,"/",year(ActStartD))),
         End_Spawn = if_else(End_Spawn < Start_spawn, End_Spawn + years(1), End_Spawn ),
         Spawn_type = ifelse((((ActStartD < Start_spawn) & (ActStartD > End_Spawn)) | is.na(Start_spawn)), "Spawn", "Not_Spawn"),
         Violation = ifelse(Spawn_type == "Spawn" & Result_cen > 13, 1,
                            ifelse(Spawn_type == "Not_Spawn" & Result_cen > Temp_C, 1, 0))
         
         ) 



