#' fit Excess Relative Risk Model 
#' 
#' function that calls the optimization (mle from stats4 package, so use optim) from an event format data set, and return a rERR object with the estimation and summary
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data data set returned from f_to_model_data
#' @param id_name name of variable containing the names of subjects
#' @param dose_name name of variable containing the doses at each time
#' @param time_name name of the time variable
#' @param covars_names a character vector with the names of the variables used as covariates in the fomrula (adjustments and stratification)
#' @param lag latency period
#' @param exclusion_done a logical indicating wheather the exclusion is already done or not
#' @return rERR object with the estimation
#' @examples
#' # set the formulas for the models
#' formula1  <- Surv(entry_age,exit_age,outcome) ~ lin(dose_cum) + strata(sex)
#' formula2  <- Surv(entry_age,exit_age,outcome) ~ loglin(factor(country)) + lin(dose_cum) + strata(sex)
#' 
#' # fit the models
#' fit1 <- f_fit_linERR_ef(formula1,data=cohort_ef,id_name="id",dose_name="dose",time_name="age",covars_names=c("sex"),lag=2,exclusion_done=T)
#' fit2 <- f_fit_linERR_ef(formula2,data=cohort_ef,id_name="id",dose_name="dose",time_name="age",covars_names=c("sex","country"),lag=2,exclusion_done=T)
#' 
#' # display a summary
#' summary(fit1)
#' summary(fit2)
#'
#' # confidence intervals
#' confint(fit1)
#' confint(fit2)
#'
#' # likelihood ratio test between nested and nesting models
#' f_lrt(fit1,fit2)
#' @export
f_fit_linERR_ef <- function (formula, data, id_name, dose_name, time_name, covars_names,lag,exclusion_done=F) 
{
  if(!exclusion_done)
    data        <- f_exclusion(formula,data,lag)
  
  form          <- f_parse_formula(formula)
  entry_name    <- as.character(form$Surv$entry)
  exit_name     <- as.character(form$Surv$exit)
  outcome_name  <- as.character(form$Surv$outcome)
  dt1           <- f_to_event_table_ef_v2(id = id_name, start = entry_name, 
                                          stop = exit_name, outcome = outcome_name, data = data, 
                                          doses = dose_name, times = time_name, covars = covars_names)
  
  dt2           <- f_to_model_data(formula, data=dt1, id_name, time_name)
  n_lin_vars    <- attr(dt2, "n_lin_vars")
  n_loglin_vars <- attr(dt2, "n_loglin_vars")
  rsets         <- f_risksets(formula, data = dt2, lag, id_name, time_name)
  fit           <- f_fit_linERR(formula, data = dt2, rsets, n_lin_vars, 
                                n_loglin_vars, id_name, time_name)
  return(fit)
}