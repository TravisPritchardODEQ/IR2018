# This code gets spawn dates for assessment units found in InputRaw
# There can be multiple spawn dates per assessment unit
# Need to figure out what spawn date we are going to use for attains
# And then join the tables back together.

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

#Query DOSat from AWQMS


Query <- glue::glue_sql("SELECT DISTINCT InputRaw.AU_ID, 
InputRaw.SpawnCode as Temp_SpawnCode, 
LU_Spawn.SpawnStart as Temp_SpawnStart, 
LU_Spawn.SpawnEnd as Temp_SpawnEnd, 
LU_Spawn.Spawn_dates as Temp_Spawn_dates
FROM     InputRaw INNER JOIN
                  LU_Spawn ON InputRaw.SpawnCode = LU_Spawn.SpawnCode", .con = con)

AU_Temp_Spawn <- DBI::dbGetQuery(con, Query)

AU_Temp_Spawn <-  AU_Temp_Spawn %>%
mutate(start_rank = case_when(Temp_SpawnStart == '8/1'~ 1,
                              Temp_SpawnStart == "8/15" ~ 2,
                              Temp_SpawnStart == '9/1' ~ 3,
                              Temp_SpawnStart == '9/15' ~ 4,
                              Temp_SpawnStart == '10/1' ~ 5,
                              Temp_SpawnStart == '10/15' ~ 6,
                              Temp_SpawnStart == '1/1' ~ 7,
                              Temp_SpawnStart == '4/1'~ 8,
                              Temp_SpawnStart == '11/1'~ 9)) %>%
  mutate(end_rank = case_when(Temp_SpawnEnd == '3/31' ~ 1,
                              Temp_SpawnEnd == '5/15' ~ 2,
                              Temp_SpawnEnd == '6/15' ~ 3,
                              Temp_SpawnEnd == '7/15' ~ 4))

Query <- glue::glue_sql("SELECT DISTINCT InputRaw.AU_ID, 
InputRaw.SpawnCode as DO_SpawnCode, 
LU_Spawn.SpawnStart as DO_SpawnStart, 
LU_Spawn.SpawnEnd as DO_SpawnEnd, 
LU_Spawn.Spawn_dates as DO_Spawn_dates
FROM     InputRaw INNER JOIN
                  LU_Spawn ON InputRaw.DO_SpawnCode = LU_Spawn.SpawnCode", .con = con)

AU_DO_Spawn <- DBI::dbGetQuery(con, Query)  
              
AU_DO_Spawn <-  AU_DO_Spawn %>%
                mutate(start_rank = case_when(DO_SpawnStart == '8/1'~ 1,
                                 DO_SpawnStart == "8/15" ~ 2,
                                 DO_SpawnStart == '9/1' ~ 3,
                                 DO_SpawnStart == '9/15' ~ 4,
                                 DO_SpawnStart == '10/1' ~ 5,
                                 DO_SpawnStart == '10/15' ~ 6,
                                 DO_SpawnStart == '1/1' ~ 7,
                                 DO_SpawnStart == '4/1'~ 8,
                                 DO_SpawnStart == '11/1'~ 9)) %>%
                mutate(end_rank = case_when(DO_SpawnEnd == '3/31' ~ 1,
                                            DO_SpawnEnd == '5/15' ~ 2,
                                            DO_SpawnEnd == '6/15' ~ 3,
                                            DO_SpawnEnd == '7/15' ~ 4))

AU_spawning_DO <- AU_DO_Spawn %>%
                   group_by(AU_ID) %>%
                   summarise(total_samples = n(),
                             n_nospawn = sum(DO_Spawn_dates =="X (No Spawning)"),
                             n_spawn = sum(!DO_Spawn_dates =="X (No Spawning)")) %>%
                   filter(n_spawn > 0)

AU_spawning_Temp <- AU_Temp_Spawn %>%
  group_by(AU_ID) %>%
  summarise(total_samples = n(),
            n_nospawn = sum(Temp_Spawn_dates =="X (No Spawning)"),
            n_spawn = sum(!Temp_Spawn_dates =="X (No Spawning)")) %>%
  filter(n_spawn > 0)

AU_Spawn_use <- rbind(AU_spawning_DO,AU_spawning_Temp) %>% distinct()
