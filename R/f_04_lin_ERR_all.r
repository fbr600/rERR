#' fit Excess Relative Risk Model 
#' 
#' function that calls the optimization (mle from stats4 package, so use optim), and return a rERR object with the estimation and summary
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data data set returned from f_to_model_data
#' @param id_name name of variable containing the names of subjects
#' @param time_name name of the time variable
#' @param lag latency period
#' @return rERR object with the estimation
#' @examples \donotrun{ f_fit_linERR_all(formula,data,id_name,time_name,lag)}
#' @export
f_fit_linERR_all <- function(formula,data,id_name,time_name,lag)
{
  # keep just model vars and expand if categorical
  dt2 <- f_to_model_data(formula,data,id_name,time_name)
  n_lin_vars    <- attr(dt2,"n_lin_vars")
  n_loglin_vars <- attr(dt2,"n_loglin_vars")
  
  # risksets
  rsets <- f_risksets(formula,data=dt2,lag,id_name,time_name)
  
  # fit the model
  fit <- f_fit_linERR(formula,data=dt2,rsets,n_lin_vars,n_loglin_vars,id_name,time_name)
  
  return(fit)
}
