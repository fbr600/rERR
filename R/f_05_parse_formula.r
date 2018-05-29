#' Parse formula (internal use)
#'
#' Return list with the terms and elements of the formula
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+\cr
#'                     lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @return list of terms in the formula
#' @examples \dontrun{ f_parse_formula(formula)}
#' @export
f_parse_formula <- function(formula)
{
  # formula left side
  formula_sv <- formula[[2]]

  # remove all pre existent variables
  if(exists("formula_lin"))    rm(formula_lin)
  if(exists("formula_loglin")) rm(formula_loglin)
  if(exists("formula_strat"))  rm(formula_strat)

  if(exists("lin_vars"))    rm(lin_vars)
  if(exists("loglin_vars")) rm(loglin_vars)
  if(exists("strata_vars")) rm(strata_vars)

  # number of terms in the right side part of the formula
  formula_terms <- sum(gregexpr("+",paste0(as.character(formula[[3]]),collapse=""), fixed=TRUE)[[1]] > 0)+1

  # splitting the formula terms of right side: linear / loglinear / strata
  if(formula_terms==1)
  {
    if(any(grepl("lin",formula[[3]])))
      formula_lin <- formula[[3]]
    if(any(grepl("logl",formula[[3]])))
      formula_loglin <- formula[[3]]
    if(any(grepl("strata",formula[[3]])))
      formula_strat <- formula[[3]]
    if(exists("formula_lin") & exists("formula_loglin"))
      if(formula_lin==formula_loglin)
        rm(formula_lin)
  }
  if(formula_terms==2)
  {
    for(i in 2:3)
    {
      if(any(grepl("lin",formula[[3]][[i]])))
        formula_lin <- formula[[3]][[i]]
      if(any(grepl("logl",formula[[3]][[i]])))
        formula_loglin <- formula[[3]][[i]]
      if(any(grepl("strata",formula[[3]][[i]])))
        formula_strat <- formula[[3]][[i]]
    }
    if(exists("formula_lin") & exists("formula_loglin"))
      if(formula_lin==formula_loglin)
        rm(formula_lin)
  }
  if(formula_terms==3)
  {
    if(any(grepl("lin",formula[[3]][[3]])))
      formula_lin <- formula[[3]][[3]]
    if(any(grepl("logl",formula[[3]][[3]])))
      formula_loglin <- formula[[3]][[3]]
    if(any(grepl("strata",formula[[3]][[3]])))
      formula_strat <- formula[[3]][[3]]

    for(i in 2:3)
    {
      if(any(grepl("lin",formula[[3]][[2]][[i]])))
        formula_lin <- formula[[3]][[2]][[i]]
      if(any(grepl("logl",formula[[3]][[2]][[i]])))
        formula_loglin <- formula[[3]][[2]][[i]]
      if(any(grepl("strata",formula[[3]][[2]][[i]])))
        formula_strat <- formula[[3]][[2]][[i]]
    }
  }

  # assumption that there is some linear part
  if(exists("formula_lin"))
  {
    lin_vars <- unlist(strsplit(as.character(formula_lin)[2:length(formula_lin)],split = "+",fixed=T))
    lin_vars <- gsub(" ","",lin_vars)
  }
  if(exists("formula_loglin"))
  {
    loglin_vars <- unlist(strsplit(as.character(formula_loglin)[2:length(formula_loglin)],split = "+",fixed=T))
    loglin_vars <- gsub(" ","",loglin_vars)
  }
  if(exists("formula_strat"))
  {
    strata_vars <- unlist(strsplit(as.character(formula_strat)[2:length(formula_strat)],split = "+",fixed=T))
    strata_vars <- gsub(" ","",strata_vars)
  }

  res <- list()

  res[[1]] <- list(entry=formula_sv[[2]],exit=formula_sv[[3]],outcome=formula_sv[[4]])
  if(exists("formula_lin")) {res[[2]] <- lin_vars
  } else res[[2]] <- character(0)

  if(exists("formula_loglin")) {res[[3]] <- loglin_vars
  } else res[[3]] <- character(0)

  if(exists("formula_strat")) {res[[4]] <- strata_vars
  } else res[[4]] <- character(0)

  res[[5]] <- character(0)
  res[[6]] <- character(0)

  if(exists("formula_lin"))
  {
    res[[5]] <- character(0)
    for(i in 1:length(lin_vars))
    {
      if(grepl("factor",lin_vars[i]))
        res[[5]] <- append(res[[5]],substr(lin_vars[i],8,nchar(lin_vars[i])-1))
    }
  }
  if(exists("formula_loglin"))
  {
    res[[6]] <- character(0)
    for(i in 1:length(loglin_vars))
    {
      if(grepl("factor",loglin_vars[i]))
        res[[6]] <- append(res[[6]],substr(loglin_vars[i],8,nchar(loglin_vars[i])-1))
    }
  }
  for(i in 1:length(res))
    if(is.null(res[[i]]) | length(res[[i]])==0)
      res[[i]] <- NA
  names(res) <- c("Surv","lin_vars","loglin_vars","strata_vars","lin_factor","loglin_factor")

  # remove all pre existent variables
  if(exists("formula_lin"))    rm(formula_lin)
  if(exists("formula_loglin")) rm(formula_loglin)
  if(exists("formula_strat"))  rm(formula_strat)

  if(exists("lin_vars"))    rm(lin_vars)
  if(exists("loglin_vars")) rm(loglin_vars)
  if(exists("strata_vars")) rm(strata_vars)


  return(res)
}
