#' Censored Data
#' 
#' This function deals with censored data as described in the white paper "Use of Censored Data" from 5/11/2018
#' When the QL > criteria value, 1/2 the value of the criteria is substituted for any sample reported as censored
#' When the QL < Criteria value, 1/2 the QL value will be substituted
#' Samples greater than the Max QL, use QL value
#' Input is a dataframe
#' Adds a column named Result_cen for the value to use
#' Column names should be surronded by backticks `` 
#' @param df dataframe to modify. 
#' @param res column in dataframe with the result to be compared against. Defaults to Result4IR
#' @param resqual column in dataframe for the qualifier. Defaults to ResultOp4IR
#' @param crit column in dataframe with criteria value. Defaults to crit
#' @export
#' 

   Censored_data <- function(df, res= `IRResultNWQSunit`, resqual = `Result_Operator`, crit = `crit`) {
    res =  enquo(res)
    resqual = enquo(resqual)
    crit  = enquo(crit)
     
     
     # Perform censored data modifications
      Results_censored <- df %>%
      # Get lowest criteria value to set censored results
      mutate( Result_cen = ifelse(UQ(resqual) == "=", as.numeric(UQ(res)),
                                 ifelse(UQ(resqual) == ">", as.numeric(UQ(res)), 
                                        ifelse(UQ(resqual) == "<", ifelse(UQ(res) > as.numeric(UQ(crit)), 0.5 * as.numeric(UQ(crit)) , 0.5 * as.numeric(UQ(res)) ), "ER" )))) %>%
        mutate(Result_cen = as.numeric(Result_cen))
  
      return(Results_censored)
      
   }


