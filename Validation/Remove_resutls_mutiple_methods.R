# analysis for removing data where the same sample was analyzed by two different methods

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")

# this table was created using the SQL script - \\deqlead-lims\AWQMS\IRDatebase\FindResultswithMultipleMethods.sql
db_qry <- glue::glue_sql( "SELECT *
                          FROM [IntegratedReport].[dbo].[Duplcate_Method_Analysis]", .con = con)

Results_import <-  DBI::dbGetQuery(con, db_qry)


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

write.csv(method_organics,"\\deqlead-lims\\AWQMS\\IRDatabase\\char_method.csv")

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
