#'DO_dup_remover
#'
#' This function removes duplcated DO data from further analysis. AWQMS has some
#' duplicate DOsat values that needs to be removed. Until this is sorted out,
#' DO concentration values get duplcaied when the code joins them with the 
#' duplcaied DOsat value. This function will identify these duplcate values
#' and keep only the first instance. 
#' Input is a dataframe and a save location for the saved duplicate file
#' @param df Dataframe to search for duplicates. This should be run right 
#'           before the IR_export function of the data
#' @param filename location and filename for the csv of generated duplcate values
#' @export
#'          


DO_Dup_remover <- function(df, filename = "Parameters/DO/Duplicated_DOsat_data.csv" ){

dups <- df %>%
  group_by(Result_UID) %>%
  filter(n()>1) %>%
  select(OrganizationID, MLocID,AU_ID, OWRD_Basin, SampleStartDate, SampleStartTime, Statistical_Base,Result_UID, IRResultNWQSunit)

if(nrow(dups) > 0){
  print("DO sat duplicates found")
  print(paste(nrow(dups), "duplicates in",length(unique(dups$Result_UID)), "UIDs"))
  print("Writing csv")
  write.csv(dups, filename, row.names = FALSE)
} else {
  print('No DO sat dups found')  
  
  }



remove_dups <- df %>%
  group_by(Result_UID) %>%
  filter(row_number()==1) 

return(remove_dups)

}