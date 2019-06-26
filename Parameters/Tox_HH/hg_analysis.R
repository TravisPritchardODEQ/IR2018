options(scipen = 999)

fun_Tox_HH_tissue_hg_analysis <- function(df){ 
  
  analysis <- df
  
  IR_export(analysis, "Parameters/Tox_HH/Data_Review/", "Tox_HH_hg_tissue", "data")

  
  categories <- analysis %>%
    group_by(AU_ID, Char_Name,SamplingMethod) %>%
    summarise(OWRD_Basin = first(OWRD_Basin),
              crit = max(crit),
              num_samples = n(),
              evaluation_mean = case_when(max(SamplingMethod != "Composite") & n() >= 3 ~ geo_mean(Result_cen),
                                          max(SamplingMethod == "Composite")& n() >= 2 ~ mean(Result_cen)),
              analysis_comment = paste0(Result_UID, collapse = ", ")
                            ) %>%
    mutate(analysis_comment = case_when(SamplingMethod != "Composite" ~ paste("Result is the geometric mean of result_UIDs:",
                                                                         analysis_comment, 
                                                                         " - due to multiple results at same date"), 
                                        SamplingMethod == "Composite" ~ paste("Result is the arithmetic mean of result_UIDs:",
                                                                              analysis_comment, 
                                                                              " - due to multiple results at same date"),
                                        TRUE ~ "")) %>%
    ungroup() %>%
    mutate(IR_category = case_when(evaluation_mean > crit ~ 1,
                                   evaluation_mean <= crit ~ 0,
                                   is.na(evaluation_mean) ~ -1)) %>%
    group_by(AU_ID, Char_Name,OWRD_Basin,crit) %>%
    summarise(max_evaluation_mean = max(evaluation_mean, na.rm = TRUE),
              num_samples = sum(num_samples),
              IR_category = case_when(max(IR_category) == 1 ~ "Cat5",
                                      max(IR_category) == 0 ~ "Cat2",
                                      max(IR_category) == -1 ~ "Cat3"
                                          )) %>%
    mutate(Pollu_ID = "109",
           WQstd_code = "16",
           max_evaluation_mean = ifelse(is.infinite(max_evaluation_mean), NA, max_evaluation_mean )) 
  
  
  
  
  
    IR_export(categories, "Parameters/Tox_HH/Data_Review/", "Tox_HH_hg_tissue", "Categories")
}