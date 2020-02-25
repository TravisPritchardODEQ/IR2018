# analysis for removing data where the same sample was analyzed by two different methods

library(tidyverse)

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

# this table was created using the SQL script - \\deqlead-lims\AWQMS\IRDatebase\FindResultswithMultipleMethods.sql
db_qry <- glue::glue_sql( "SELECT *
                          FROM [IntegratedReport].[dbo].[Duplcate_Method_Analysis]", .con = con)
db_qry_chlordane <- glue::glue_sql("SELECT *
                          FROM [IntegratedReport].[dbo].[ResultsRawWater2018] where IRPollutantID = 27", .con = con)

Results_import <-  DBI::dbGetQuery(con, db_qry)
chlordane <- DBI::dbGetQuery(con, db_qry_chlordane)

chlordane_unused <- chordane %>%
             mutate(is_HR = ifelse(Analytical_method =='Pesticides in water, soil, sediment, biosolids, and tissue by HRGC/HRMS',1,0)) %>%
             group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia,OrganizationID) %>%
             mutate(has_HR = ifelse(max(is_HR) == 1, 1, 0)) %>%
  ungroup() %>%
  filter(has_HR == 1, is_HR == 0) %>%
  select(Result_UID,Char_Name) %>% 
  mutate(Data_Review_Code = 52) %>% 
  mutate(Data_Review_Comment = 'same sample analyzed by Pesticides in water, soil, sediment, biosolids, and tissue by HRGC/HRMS')

write.csv(chlordane_unused,"//deqlead-lims/SERVERFOLDERS/AWQMS/IRDatabase/Unused_chlordane_from_R.csv")


#filters out temp, pH, turb, cond and DO because these are coming through as dups due to continuous data - first analysis on orgaincs methods
method_organics <- Results_import %>% 
  filter(!chr_uid %in% c(985,986,1648,1977,2849, 2982)) %>%
  group_by(chr_uid, Char_Name) %>% 
  summarise(total_samples = n(),
            semivolCGC = sum(Analytical_method == 'Semivolatile Organic Compounds by CGC/MS'),
            semivolGC = sum(Analytical_method == 'Semivolatile Organic Compounds by GC/MS'),
            HR_pest = sum(Analytical_method =='Pesticides in water, soil, sediment, biosolids, and tissue by HRGC/HRMS'),
            NonVol_HPLC = sum(Analytical_method =='Non-Volatile Compounds by HPLC'),
            Vol_CGC_MS = sum(Analytical_method =='Volatile Organics by CGC/MS'),
            Cl_herb = sum(Analytical_method == 'Chlorinated Phenoxy Herbicides in Water'))

write.csv(method_organics,"//deqlead-lims/AWQMS\\IRDatabase\\char_method.csv")

## determine method/restuls with lowest available MRL for group of pesticides with two common  methods 
Pest <- Results_import %>% 
  filter(chr_uid %in% c(7,8,13,529,821,934,1001,1104,1105,1115,1235,1280,1349,1516,1517,1518,100464))
# ensure all MRLs have same units 
unique(Pest$Analytical_method) 
unique(Pest$MRLUnit)

# select result with lowest MRL
Pest <- Results_import %>% 
  filter(chr_uid %in% c(7,8,13,529,821,934,1001,1104,1105,1115,1235,1280,1349,1516,1517,1518,100464)) %>%
  group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia, chr_uid) %>%
  slice(which.min(MRLValue))
unique(Pest$Analytical_method) 

## output = [1] "Pesticides in water, soil, sediment, biosolids, and tissue by HRGC/HRMS" 
## removed results analyzed by other methods 
unused <- Results_import %>%
  mutate(Data_Review_Code = ifelse(chr_uid %in% c(7,8,13,529,821,934,1001,1104,1105,1115,1235,1280,1349,1516,1517,1518,100464) 
                                   & Analytical_method == 'Semivolatile Organic Compounds by GC/MS', 52, 1)) %>%
  mutate(Data_Review_Comment = ifelse(chr_uid %in% c(7,8,13,529,821,934,1001,1104,1105,1115,1235,1280,1349,1516,1517,1518,100464) 
                                      & Analytical_method == 'Semivolatile Organic Compounds by GC/MS',"same sample analyzed by Pesticides in water, soil, sediment, biosolids, and tissue by HRGC/HRMS", "")) %>%
  filter(Data_Review_Code == 52) %>% 
  select(Result_UID,Char_Name,Data_Review_Code,Data_Review_Comment)
# export and add to unused_data_2018 table in IR 2018 database 
write.csv(unused,"Z:\\AWQMS\\IRDatabase\\Unused_Pest_from_R.csv")

# Remove multiple method for Azinphos_methyl
Azinphos_methyl <- Results_import %>%
  filter(chr_uid == 602)
unique(Azinphos_methyl$Analytical_method) 
unique(Azinphos_methyl$MRLUnit)
# check methods for lowest MRL by sample 
Azinphos_methyl <- Results_import %>%
  filter(chr_uid == 602) %>%
  group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia) %>%
  slice(which.min(MRLValue))
unique(Azinphos_methyl$Analytical_method)
# lowest MRL fluctuates between methods 
unused_Azinphos_methyl <- Results_import %>%
  filter(chr_uid == 602) %>%
  group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia) %>%
  slice(which.max(MRLValue)) %>%
  mutate(Data_Review_Code = 52) %>%
  mutate(Data_Review_Comment = "same sample analyzed by different method with a lower detection level (MRLValue)")

unused_Azinphos_methyl <- select(unused_Azinphos_methyl,Result_UID,Char_Name,Data_Review_Code,Data_Review_Comment)
write.csv(unused_Azinphos_methyl,"Z:\\AWQMS\\IRDatabase\\Unused_Azinphos_methyl_from_R.csv")

# Remove multiple method for Pentachlorophenol
Pentachlorophenol <- Results_import %>%
  filter(chr_uid == 1633)
unique(Pentachlorophenol$Analytical_method) 
unique(Pentachlorophenol$MRLUnit)
# ugh multiple units! 
Pentachlorophenol <- Results_import %>%
  filter(chr_uid == 1633) %>% 
  mutate(MRLValue_ngL = ifelse(MRLUnit == "ug/l", MRLValue*1000,MRLValue)) 

Penta_good <- Pentachlorophenol %>%
  group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia) %>%
  slice(which.min(MRLValue_ngL))
unique(Penta_good$Analytical_method) 
# lowest MRL fluctuates between methods 
unused_penta <- Pentachlorophenol %>%
  group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia) %>%
  slice(which.max(MRLValue_ngL)) %>%
  mutate(Data_Review_Code = 52) %>%
  mutate(Data_Review_Comment = "same sample analyzed by different method with a lower detection level (MRLValue)")
unused_penta <- select(unused_penta,Result_UID,Char_Name,Data_Review_Code,Data_Review_Comment)
write.csv(unused_penta,"Z:\\AWQMS\\IRDatabase\\Unused_Pentachlorophenol_from_R.csv")

Alk <- Results_import %>% 
  filter(chr_uid == 544) %>% 
  group_by(MLocID,SampleStartDate,SampleStartTime,SampleMedia) %>%
  summarise(total_samples = n(),
            titration = sum(Analytical_method == 'Alkalinity by Titration'),
            Gran = sum(Analytical_method == 'Alkalinity by Gran Titration'))

# there are non detects choose Alkalinity by Titration method 

unused_Alk <- Results_import %>% 
  filter(chr_uid == 544) %>%
  filter(Analytical_method =='Alkalinity by Gran Titration') %>%
  mutate(Data_Review_Code = 52) %>% 
  mutate(Data_Review_Comment = "Alkalinity by Titration") %>%
  select(Result_UID,Char_Name,Data_Review_Code,Data_Review_Comment)

write.csv(unused_Alk,"Z:\\AWQMS\\IRDatabase\\Unused_Alk_from_R.csv")

unused_Arsenic <- Results_import %>% 
  filter(chr_uid == 591) %>%
  filter(Analytical_method =='Metals by Temperature Stabilized GFAA') %>%
  mutate(Data_Review_Code = 52) %>% 
  mutate(Data_Review_Comment = "same sample analyzed by Metals in Waters by ICP/MS") %>%
  select(Result_UID,Char_Name,Data_Review_Code,Data_Review_Comment)

write.csv(unused_Arsenic,"Z:\\AWQMS\\IRDatabase\\Unused_Arsenic_from_R.csv")

#### checked everything else and it was already being removed for InputRaw for another reason 


