#' Data transformation: keep model variables and expand categorical variables (internal use)
#' 
#' Transform the data set in a closed form n_row|id_name|n_pe|entry_name|exit_name|outcome|time|linear_covariates|loglinear_covariates.
#' Expand if a variable is categorical to pure logical n_categories variables (excluding the reference category)
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data data set
#' @param id_name name of variable containing the names of subjects
#' @param time_name name of the time variable
#' @return data set described below
#' @examples \donotrun{ f_to_model_data(formula,data,id_name='patientids',time_name='time')}
#' @importFrom dplyr arrange


f_to_model_data <- function(formula,data,id_name,time_name)
{
  # formula left side
  formula_sv <- formula[[2]]
  
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
    if(any(grepl("logl",formula_lin)))
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
    if(any(grepl("logl",formula_lin)))
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
    if(any(grepl("logl",formula_lin)))
      rm(formula_lin)
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
  
  # resultant data.frame
  v_id      <- eval(parse(text=paste0("data$",id_name)))
  v_n_pe    <- eval(parse(text=paste0("data$","n_pe")))
  v_entry   <- eval(parse(text=paste0("data$",formula_sv[[2]])))
  v_exit    <- eval(parse(text=paste0("data$",formula_sv[[3]])))
  v_outcome <- eval(parse(text=paste0("data$",formula_sv[[4]])))
  v_time    <- eval(parse(text=paste0("data$",time_name)))
  
  dt        <- data.frame(v_id,v_n_pe,v_entry,v_exit,v_outcome,v_time)
  names(dt) <- c(id_name,"n_pe",as.character(formula_sv[[2]]),as.character(formula_sv[[3]]),as.character(formula_sv[[4]]),time_name)
  
  # linear covariates
  n_lin_vars <- 0
  if(exists("formula_lin"))
  {
    for(i in 1:length(lin_vars))
    {
      is_factor <- F
      if(grepl("factor",lin_vars[i]))
      {
        lin_vars[i] <- substr(lin_vars[i],8,nchar(lin_vars[i])-1)
        is_factor   <- T
      }
      x <- eval(parse(text=paste0("data$",lin_vars[i])))
      if(is.factor(x) | is_factor)
      {
        x      <- as.factor(x)
        levels <- levels(x)
        for(j in 2:length(levels))
        {
          n_lin_vars            <- n_lin_vars+1
          x_lev                 <- as.numeric(x==levels[j])
          dt                    <- cbind(dt,x_lev)
          names(dt)[dim(dt)[2]] <- paste0(lin_vars[i],"_",levels[j])
        }
      }
      else
      {
        n_lin_vars            <- n_lin_vars+1
        dt                    <- cbind(dt,x)
        names(dt)[dim(dt)[2]] <- lin_vars[i]
      }
    }
  }
  
  # loglinear covariates
  n_loglin_vars <- 0
  if(exists("formula_loglin"))
  {
    for(i in 1:length(loglin_vars))
    {
      is_factor <- F
      if(grepl("factor",loglin_vars[i]))
      {
        loglin_vars[i] <- substr(loglin_vars[i],8,nchar(loglin_vars[i])-1)
        is_factor      <- T
      }
      x <- eval(parse(text=paste0("data$",loglin_vars[i])))
      if(is.factor(x) | is_factor)
      {
        x      <- as.factor(x)
        levels <- levels(x)
        for(j in 2:length(levels))
        {
          n_loglin_vars         <- n_loglin_vars+1
          x_lev                 <- as.numeric(x==levels[j])
          dt                    <- cbind(dt,x_lev)
          names(dt)[dim(dt)[2]] <- paste0(loglin_vars[i],"_",levels[j])
        }
      }
      else
      {
        n_loglin_vars         <- n_loglin_vars+1
        dt                    <- cbind(dt,x)
        names(dt)[dim(dt)[2]] <- loglin_vars[i]
      }
    }
  }
  
  # strata vars
  if(exists("formula_strat"))
  {
    for(i in 1:length(strata_vars))
    {
      x                     <- eval(parse(text=paste0("data$",strata_vars[i])))
      dt                    <- cbind(dt,x)
      names(dt)[dim(dt)[2]] <- strata_vars[i]
    }
  }
  # order by id and time:cols 1 and 6
  dt    <- cbind(dt,dt[,c(1,6)])
  names(dt)[(dim(dt)[2]-1):dim(dt)[2]] <- c("id_aux","time_aux")
  dt    <- arrange(dt,id_aux,time_aux)
  # add the rownumber
  n_row <- data.frame(n_row=1:(dim(dt)[1]))
  dt    <- cbind(n_row,dt)
  dt    <- dt[,c(1:(dim(dt)[2]-2))]
  
  attr(dt,"n_lin_vars")    <- n_lin_vars
  attr(dt,"n_loglin_vars") <- n_loglin_vars
  return(dt)
}



