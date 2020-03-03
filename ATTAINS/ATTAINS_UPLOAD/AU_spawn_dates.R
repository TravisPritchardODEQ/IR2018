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

Query <- glue::glue_sql("SELECT DISTINCT InputRaw.AU_ID, 
InputRaw.SpawnCode as DO_SpawnCode, 
LU_Spawn.SpawnStart as DO_SpawnStart, 
LU_Spawn.SpawnEnd as DO_SpawnEnd, 
LU_Spawn.Spawn_dates as DO_Spawn_dates
FROM     InputRaw INNER JOIN
                  LU_Spawn ON InputRaw.DO_SpawnCode = LU_Spawn.SpawnCode", .con = con)

AU_DO_Spawn <- DBI::dbGetQuery(con, Query)



