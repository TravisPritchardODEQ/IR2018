
DO_estuary_analysis <- function(df){
  
  
  library(lubridate)
  library(IRlibrary)
  
  
  
  # add spawn start and end dates as dates, include indicator if actdate is within spawn
  # add critical period start and end dates, include indicator is actdate is within critperiod
  Results_estuary <- df %>%
    filter(!is.null(OWRD_Basin) & DO_code == 1) %>%
    filter(Statistical_Base == "Minimum" |
             is.na(Statistical_Base) ) %>%
    mutate(SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
           SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
           SpawnStart = mdy(SpawnStart),
           SpawnEnd = mdy(SpawnEnd),
           SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd ),
           SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - years(1), # subtract a year if in spawn period carrying from previous year
                                SpawnStart),
           in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ))
    
    
    Estuary_spawn <- Results_estuary %>%
      filter(in_spawn == 1, !is.na(AU_ID)) %>%
      mutate(spawn_crit = 11,
             excursion = ifelse(IRResultNWQSunit < spawn_crit, 1, 0))
             
             
    
    
    IR_export(Estuary_spawn, "Parameters/DO/Data_Review", "DO_Estuary_Spawn", "data" )
    
    estuary_spawn_categories <- Estuary_spawn %>%
      group_by(AU_ID) %>%
      summarise(OWRD_Basin = first(OWRD_Basin), 
                num_samples = n(),
                num_excursions = sum(excursion, na.rm = TRUE)) %>%
      mutate(critical_excursions = excursions_conv(num_samples)) %>%
      mutate(IR_category = case_when(num_samples < 5 & num_excursions == 0 ~ "Cat 3",
                                     num_samples < 5 & num_excursions > 0 ~ "Cat 3B",
                                     num_samples >= 5 & num_excursions >= critical_excursions ~  "Cat 5",
                                     num_samples >= 5 & num_excursions < critical_excursions ~ "Cat 2",
                                     TRUE ~ 'ERROR')) 
    
    IR_export(estuary_spawn_categories, "Parameters/DO/Data_Review", "DO_Estuary_Spawn", "categories" )
    
    

# Yearround estuary -------------------------------------------------------

    Results_estuary_data <- Results_estuary %>%
      mutate(excursion = ifelse(IRResultNWQSunit < crit_Instant, 1, 0 )) 
    
    IR_export(Results_estuary_data, "Parameters/DO/Data_Review", "DO_Estuary_Yearround", "data" )
    
    estuary_categories <- Results_estuary_data %>%
      group_by(AU_ID) %>%
      summarise(OWRD_Basin = first(OWRD_Basin), 
                num_samples = n(),
                num_excursions = sum(excursion, na.rm = TRUE)) %>%
      mutate(critical_excursions = excursions_conv(num_samples)) %>%
      mutate(IR_category = case_when(num_samples < 5 & num_excursions == 0 ~ "Cat 3",
                                     num_samples < 5 & num_excursions > 0 ~ "Cat 3B",
                                     num_samples >= 5 & num_excursions >= critical_excursions ~  "Cat 5",
                                     num_samples >= 5 & num_excursions < critical_excursions ~ "Cat 2",
                                     TRUE ~ 'ERROR')) 
    
    IR_export(estuary_categories, "Parameters/DO/Data_Review", "DO_Estuary_Yearround", "categories" )
    
    
    return(list(estuary_spawn_categories,estuary_categories ))
}