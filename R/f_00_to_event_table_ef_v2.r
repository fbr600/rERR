#' Data transformation: Event format -> Event format required for the model (internal use)
#'
#' This function organize an input data set ef, same as f_to_event_table_ef_all but with start,stop and outcome from the formula
#' @param id name of variable containing the names of subjects
#' @param start name of the variable containing the start time
#' @param stop name of the variable containing the stop time
#' @param outcome name of the variable containging the outcome
#' @param data input data set
#' @param times name of the time variable
#' @param doses name of the dose variable
#' @param covars names of the covars required later in the model
#' @return The data set with the event-row format, including the event of exit of the cohort
#' @examples \donotrun{ f_to_event_table_ef_v2(id='patientids',start='entry_age',stop='exit_age',outcome='leukaemia',data,times='age',doses='ActMar_med',covars=c('sex','country,'birthcohort'))}
#' @importFrom plyr count
#' @importFrom dplyr group_by 
#' @importFrom dplyr mutate 
#' @importFrom dplyr arrange 
#' @importFrom dplyr "%>%"

f_to_event_table_ef_v2 <- function(id,start,stop,outcome,data,times,doses,covars)
{
  call <- match.call()
  
  # to do
  # id_name   <- eval(call$id)
  # dose_name <- eval(call$doses)
  # stop_name <- eval(call$stop)
  # time_name <- eval(call$times)
  
  id_name   <- eval(id)
  dose_name <- eval(doses)
  stop_name <- eval(stop)
  time_name <- eval(times)
  
  # create the n_pe
  data$id_ <- eval(parse(text=paste0(call$data,"$",id_name)))
  data     <- data    %>%
    group_by(id_)     %>%
    dplyr::mutate(n_pe=1:length(id_))
  
  # add the exit event with the outcome at this event
  dt                                 <- data[which(data$n_pe==1),]
  dt$n_pe                            <- 0
  dt[,which(names(dt)==dose_name)]   <- 0
  dt[,which(names(dt)==time_name)]   <- eval(parse(text=paste0("dt$",stop_name)))
  data[,which(names(data)==outcome)] <- 0
  data                               <- rbind(data,dt)
  data$time_aux                      <- eval(parse(text=paste0("data$",time_name)))
  data                               <- arrange(data,id_,time_aux)
  data                               <- data[,-dim(data)[2]]
  
  # create the dose_cum
  dose_num      <- eval(parse(text=paste0("data$",dose_name)))
  data$dose_num <- dose_num
  data          <- data %>%
    group_by(id_)       %>%
    dplyr::mutate(dose_cum=cumsum(dose_num))
  
  a        <- plyr::count(data$id_)
  data$id1 <- unlist(lapply(seq_along(1:dim(dt)[1]),function(x) rep(x,each=a$freq[x])))
  
  return(data)
}