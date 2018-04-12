#' Data transformation: Event format -> Event format required for the model
#'
#' This function organize an input data set ef (event format), to the required ef data set for the model.
#' It appends an index of person event 1,2..n,0 if a subject has n doses and being the 0-row the exit conditions: exit time, exit dose and the outcome.
#' Also creates the cumulated dose.
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data input data set - event format data set
#' @param id_name name of variable containing the names of subjects
#' @param dose_name name of the dose variable
#' @param time_name name of the time variable
#' @param covars_names names of the covars required later in the model
#' @return The data set with the event-row format, including the event of exit of the cohort as a row where the outcome is set
#' @examples \donotrun{ f_to_event_table_ef_all(formula,data,id_name='patientids',dose_name='dose',time_name='time',covars=c('sex','country,'birthcohort'))}
#' @export
f_to_event_table_ef_all <- function(formula,data,id_name,dose_name,time_name,covars_names)
{
  form          <- f_parse_formula(formula)
  entry_name    <- as.character(form$Surv$entry)
  exit_name     <- as.character(form$Surv$exit)
  outcome_name  <- as.character(form$Surv$outcome)

  dt1           <- f_to_event_table_ef_v2(id = id_name,start=entry_name,stop=exit_name,outcome=outcome_name,
                                          data = data,doses = dose_name,times = time_name,covars = covars_names)

  return(dt1)
}


