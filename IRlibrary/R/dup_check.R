#' dup_check
#' 
#' This function perfroms a duplicate check based on 
#' MLocID, ActStartD, ActStartT, ChrName and Results
#' 
#' @param chr parameter name used for output filename only
#' @param df dataframe for dup check. This should be the dataframe imported from IR database
#' @export
#' @examples  


dup_check <- function(df, chr) {
  
  
  Dup_data <- df %>%
    group_by(MLocID, ActStartD, ActStartT, Char_Name, IRResultNWQSunit) %>%
    summarise(count = n()) %>%
    filter(count > 1)
  
  write.csv(Dup_data, paste0("/Parameters/", chr, "dup_data.csv"))
}