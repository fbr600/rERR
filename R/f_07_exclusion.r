#' Exclusion period related with the latency - lag period
#' 
#' Exclude subjects in the cohort that have less than lag time of follow up
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data initial data set
#' @param lag latency period
#' @return a data set with the exclusion updated
#' @examples \donotrun{ f_exclusion(formula,data,lag)}

f_exclusion <- function(formula,data,lag)
{
  # parse entry and exit names
  form          <- f_parse_formula(formula)
  entry_name    <- as.character(form$Surv$entry)
  exit_name     <- as.character(form$Surv$exit)
  
  # evaluate entry and exit from the data set
  entry <- data[,which(names(data)==entry_name)]
  exit  <- data[,which(names(data)==exit_name)]
  
  # apply the exclusion criterium
  entry <- entry + lag
  
  # set the entry time to entry+lag
  data[,which(names(data)==entry_name)] <- entry
  
  # select the rows that accomplish the condition
  rows_to_keep              <- which(entry<exit)
  data                      <- data[rows_to_keep,]
  attr(data,"rows_to_keep") <- rows_to_keep
  # return data set
  return(data)
}
