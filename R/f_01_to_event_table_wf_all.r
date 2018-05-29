#' Data transformation: Wide format -> Event format required for the model
#'
#' This function organize an input data set wf (wide format, same input as in Epicrue Peanuts), to the required ef data set for the model.
#' It appends an index of person event 1,2..n,0 if a subject has n doses (or grouped doses), and being the 0-row the exit conditions: exit time, exit dose and the outcome.
#' Also creates the cumulated dose.
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+\cr
#'                     lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data input data set - event format data set
#' @param id_name name of variable containing the names of subjects
#' @param doses sub data set of grouped doses
#' @param times sub data set of times relatives to doses
#' @param covars sub data set of the covars that will be involved in the model
#' @return The data set with the event-row format, including the event of exit of the cohort
#' @examples \dontrun{f_to_event_table_wf_all(formula,data,id_name='patientids',doses=data[,31:50],\cr
#'                              times=data[,11:30],covars=data[,c('sex','country','birthcohort')])}
#' @export

f_to_event_table_wf_all <- function(formula,data,id_name,doses,times,covars)
{
  form          <- f_parse_formula(formula)
  entry_name    <- as.character(form$Surv$entry)
  exit_name     <- as.character(form$Surv$exit)
  outcome_name  <- as.character(form$Surv$outcome)

  dt1           <- f_to_event_table_wf_v2(id = id_name,start=entry_name,stop=exit_name,outcome=outcome_name,
                                          data = data,doses = doses,times = times,covars = covars)

  return(dt1)
}
