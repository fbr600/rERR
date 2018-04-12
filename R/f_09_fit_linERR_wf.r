#' fit Excess Relative Risk Model 
#' 
#' function that calls the optimization (mle from stats4 package, so use optim) from an event format data set, and return a rERR object with the estimation and summary
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data data set returned from f_to_model_data
#' @param id_name name of variable containing the names of subjects
#' @param doses sub data set of grouped doses
#' @param times sub data set of times relatives to doses
#' @param covars sub data set of the covars that will be involved in the model (adjustments and stratification)
#' @param lag latency period
#' @param exclusion_done a logical indicating wheather the exclusion is already done or not
#' @return rERR object with the estimation
#' @examples 
#' # set the formulas for the models
#' formula1  <- Surv(AgeAtEntry,age_at_event,outcome) ~ lin(dose_cum) + strata(sex)
#' formula2  <- Surv(AgeAtEntry,age_at_event,outcome) ~ loglin(factor(country)) + lin(dose_cum) + strata(sex)
#' 
#' # fit the models
#' fit1 <- f_fit_linERR_wf(formula1,data=cohort_wf,id_name="id",doses=cohort_wf[,45:79],times=cohort_wf[,10:44],
#'                         covars=cohort_wf[,c("sex","country")],lag=2,exclusion_done = F)
#' 
#' fit2 <- f_fit_linERR_wf(formula2,data=cohort_wf,id_name="id",doses=cohort_wf[,45:79],times=cohort_wf[,10:44],
#'                         covars=cohort_wf[,c("sex","country")],lag=2,exclusion_done = F)
#' 
#' # display a summary
#' summary(fit1)
#' summary(fit2)
#'
#' # confidence intervals
#' confint(fit1)
#' confint(fit2) 
#'
#' # likelihood ratio test between nested and nesting models#' 
#' f_lrt(fit1,fit2)
#' 
#' @export
f_fit_linERR_wf <- function (formula, data, id_name, doses,times, covars,lag,exclusion_done=F) 
{
  if(!exclusion_done)
  {
    
    data           <- f_exclusion(formula,data,lag)
    doses          <- doses[attr(data,"rows_to_keep"),]
    times          <- times[attr(data,"rows_to_keep"),]
    covars         <- covars[attr(data,"rows_to_keep"),]
  }
  form         <- f_parse_formula(formula)
  entry_name   <- as.character(form$Surv$entry)
  exit_name    <- as.character(form$Surv$exit)
  outcome_name <- as.character(form$Surv$outcome)
  dt1          <- f_to_event_table_wf_v2(id = id_name, start = entry_name, 
                                         stop = exit_name, outcome = outcome_name, data = data, 
                                         doses = doses, times = times, covars = covars)
  
  dt2           <- f_to_model_data(formula, data=dt1, id_name, time_name="time")
  n_lin_vars    <- attr(dt2, "n_lin_vars")
  n_loglin_vars <- attr(dt2, "n_loglin_vars")
  rsets         <- f_risksets(formula, data = dt2, lag, id_name, time_name="time")
  fit           <- f_fit_linERR(formula, data = dt2, rsets, n_lin_vars, 
                                n_loglin_vars, id_name, time_name="time")
  return(fit)
}