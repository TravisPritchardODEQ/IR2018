options(scipen = 999)

fun_Tox_HH_tissue_hg_analysis <- function(df){ 
  
  analysis <- df
  
  IR_export(analysis, "Parameters/Tox_HH/Data_Review/", "Tox_HH_hg_tissue", "data")

  
  categories <- analysis %>%
    mutate(is_composite = case_when(grepl("composite", SamplingMethod, ignore.case = TRUE) ~ 1,
                                    grepl("composite", Activity_Type, ignore.case = TRUE) ~ 1,
                                    TRUE ~ 0)) %>%
    group_by(AU_ID, Char_Name,is_composite) %>%
    summarise(OWRD_Basin = first(OWRD_Basin),
              crit = max(crit),
              num_samples = n(),
              evaluation_mean = case_when(max(is_composite != 1) & n() >= 3 ~ geo_mean(Result_cen),
                                          max(is_composite == 1) & n() >= 2 ~ mean(Result_cen)),
              analysis_comment = paste0(Result_UID, collapse = ", ")
                            ) %>%
    mutate(analysis_comment = case_when(is_composite != 1 ~ paste("Result is the geometric mean of result_UIDs:",
                                                                         analysis_comment), 
                                        is_composite == 1 ~ paste("Result is the arithmetic mean of result_UIDs:",
                                                                              analysis_comment),
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