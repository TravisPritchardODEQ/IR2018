library(IRlibrary)

penta_data_analysis <- Penta_data %>%
  mutate(violation = ifelse(Result_cen > CMC_crit, 1, 0 ))


#Write table here

penta_data_summary <- penta_data_analysis %>%
  group_by(AU_ID) %>%
  summarise(num_samples = n(),
            num_Violations = sum(violation)) %>%
  mutate(critical_excursions = excursions_tox(num_samples))

