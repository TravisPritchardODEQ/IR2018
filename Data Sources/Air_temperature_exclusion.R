library(tidyverse)
library(lubridate)
library(zoo)

# Filepath where air temperture files are located
filepath <- "//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Data Call/Air Temperature/"


# Get vector of filenames
# Only include .csv files
in_fnames <- list.files(filepath, full.names = TRUE)
in_fnames <- in_fnames[grep(".csv", in_fnames)]
in_fnames <- in_fnames[!grepl("90", in_fnames)]
in_fnames <- in_fnames[!grepl("yearly", in_fnames)]

# Create list to get data out of loop
data_list <- list()


# Loop through files and add data to list
for(i in 1:length(in_fnames)){
  
print(paste("file", i, "of",length(in_fnames)  ))
  
data_import <- read.csv(in_fnames[i], stringsAsFactors = FALSE)

data_list[[i]] <- data_import

}

# Bind all data to one dataframe
oregon_temp_data <- bind_rows(data_list) %>%
  filter(!is_empty(DATE))


# Remove uncessesary items
rm(data_list)
rm(data_import)


# # Calculate 7 day average maximum air temps -----------------------------

air_temp_calcs <- oregon_temp_data %>%
  mutate(DATE = mdy(DATE),
         YEAR = year(DATE),
         crit_start = mdy(paste0("7/1/", YEAR)),
         crit_end = mdy(paste0("9/30/", YEAR)),
         in_crit_period = ifelse(DATE >= crit_start & DATE <= crit_end , 1, 0 )) %>%
  filter(!is.na(YEAR)) %>%
  arrange(STATION, DATE) %>%
  group_by(STATION) %>%
  mutate(startdate7 = lag(DATE, 6, order_by = DATE),
         calc7ma = ifelse(startdate7 == (as.Date(DATE) - 6), 1, 0 )) %>%
  mutate(ma.max7 = ifelse(calc7ma == 1 ,round(rollmean(x = TMAX, 7, align = "right", fill = NA),0) , NA )) %>%
  ungroup() %>%
  group_by(STATION, YEAR) %>%
  summarise(max.ma.max7 = max(ma.max7, na.rm = TRUE),
            total.values = n(),
            total.critical = sum(in_crit_period)) %>%
  ungroup() %>%
  group_by(STATION) %>%
  mutate(num.years = n())

#write.csv(air_temp_calcs,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Data Call/Air Temperature/yearly_max_7dmax.csv" )

air_temp_90_percentile <- air_temp_calcs %>%
  group_by(STATION) %>%
  summarise(per90 = as.numeric((quantile(max.ma.max7, c(0.90)))),
            num.years = first(num.years),
            total.results = sum(total.values),
            total.possible.results = 365*num.years,
            total.percent = round(total.results/total.possible.results,2),
            total.crit.results = sum(total.critical),
            total.possible.crit.results = 92*num.years,
            critical.percentage = round(total.crit.results/ total.possible.crit.results,2)
            ) 


#write.csv(air_temp_90_percentile,"//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Data Call/Air Temperature/90thpercentilevalues.csv", row.names = FALSE )



#data exploration


data_explore <- oregon_temp_data %>%
  filter(NAME == "BANDON 2 NNE, OR US")


air_temp_90 <- air_temp_90_percentile %>%
  select(STATION, per90)


data_explore <- oregon_temp_data %>%
  mutate(DATE = mdy(DATE),
         YEAR = year(DATE),
         crit_start = mdy(paste0("7/1/", YEAR)),
         crit_end = mdy(paste0("9/30/", YEAR)),
         in_crit_period = ifelse(DATE >= crit_start & DATE <= crit_end , 1, 0 )) %>%
  filter(!is.na(YEAR)) %>%
  arrange(STATION, DATE) %>%
  group_by(STATION) %>%
  mutate(startdate7 = lag(DATE, 6, order_by = DATE),
         calc7ma = ifelse(startdate7 == (as.Date(DATE) - 6), 1, 0 )) %>%
  mutate(ma.max7 = ifelse(calc7ma == 1 ,round(rollmean(x = TMAX, 7, align = "right", fill = NA),0) , NA )) %>%
  gather(key = "Type", value = "Value", TMAX, ma.max7) %>%
  left_join(air_temp_90, by = "STATION")
 
  

for(i in 1:length(unique(data_explore$STATION))){
  
  stat <- unique(data_explore$STATION)[i]
  nm <- unique(data_explore$NAME)[i]
  
  data_graph <- data_explore %>%
    filter(STATION == stat)
  
  per <- data_graph$per90[1]
  
  yr.max.ma.max7 <- air_temp_calcs %>%
    filter(STATION == stat) %>%
    select(STATION, YEAR, max.ma.max7)
  
  
  g <- ggplot()+
    geom_density(data = data_graph, aes(x = Value, fill = Type), alpha = 0.7) +
    geom_point(data = yr.max.ma.max7, aes(x = max.ma.max7, y = 0, size = ""), alpha = 0.8, color = "steelblue4") +
    geom_vline(xintercept = per)+
    xlab("Temperature")+
    theme_classic()+
    #scale_fill_discrete(labels = c("7 Day Avg Max", "Daily Max")) +
    scale_fill_manual(labels = c("7 Day Avg Max", "Daily Max"), values = c("#E69F00", "#999999"), guide= guide_legend(override.aes = list(shape = NA))) +
    guides(size=guide_legend(title="Yearly maximum 7 day average", order = 2),
           fill = guide_legend(title="Temperature Metric", order = 1)) +
    ggtitle(paste(stat, " - ", nm), subtitle = ("Air Temp" )) +
    theme(legend.title = element_text(size=8))
  
  
  ggsave(paste0("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Data Call/Air Temperature/graphs/",stat,".png"), g, device = "png")
  
}    


