#' ata transformation: Wide format -> Event format required for the model (internal use)
#'
#' This function organize an input data set wf (wide format) to ef(event format), same as f_to_event_table_wf_all but with the start, stop and outcome from the formula
#' @param id name of variable containing the names of subjects
#' @param start names of the variable containing the start time 
#' @param stop names of the variable containing the stop time 
#' @param outcome name of the variable containing the outcome
#' @param data input data set - wide format data set
#' @param times sub data set of times relatives to doses
#' @param doses sub data set of grouped doses
#' @param covars sub data set of the covars that will be involved in the model
#' @return The data set with the event-row format, including the event of exit of the cohort
#' @examples \donotrun{ f_to_event_table_wf_v2(id='patientids',start='entry_age',stop='exit_age',outcome='leukaemia',data,times=data[,11:30],doses=data[,31:50],covars=data[,c('sex','country,'birthcohort')])}
#' @importFrom plyr count
#' @importFrom dplyr group_by 
#' @importFrom dplyr mutate 
#' @importFrom dplyr arrange 
#' @importFrom dplyr "%>%"


f_to_event_table_wf_v2 <- function(id,start,stop,outcome,
                                   data,times,doses,covars)
{
  call <- match.call()
  
  start_   <- eval(parse(text=paste0(call$data,"$",start)))
  stop_    <- eval(parse(text=paste0(call$data,"$",stop)))
  outcome_ <- eval(parse(text=paste0(call$data,"$",outcome)))
  
  # big n: all possible person years
  n_sub       <- dim(data)[1]
  n_exp_years <- dim(times)[2]
  n           <- n_sub*n_exp_years
  
  # list of variables: id|start|stop|outcome|covariates|time|dose|cumulative dose|person_event|row_number in original data set
  names  <- c(as.character(id),as.character(start),as.character(stop),as.character(outcome),
              names(covars),"time","dose","dose_cum","n_pe","id1")
  
  n_cols <- length(names)
  
  # new dataset
  mat <- matrix(nrow=n,ncol=n_cols)
  
  mat <- as.data.frame(mat)
  mat[,1] <- rep(eval(parse(text=paste0(call$data,"$",id))),each=n_exp_years)
  mat[,2] <- rep(start_,each=n_exp_years)
  mat[,3] <- rep(stop_,each=n_exp_years)
  mat[,4] <- 0
  
  for(i in 1:dim(covars)[2])
    mat[,4+i] <- rep(covars[,i],each=n_exp_years)
  
  names(mat) <- names
  
  # times
  x <- apply(times,1,paste,collapse=" ")
  x <- strsplit(x," ")
  x <- lapply(x,as.numeric)
  x <- unlist(x)
  
  mat[,which(names(mat)=="time")] <- x
  
  # doses
  x <- apply(doses,1,paste,collapse=" ")
  x <- strsplit(x," ")
  x <- lapply(x,as.numeric)
  x <- unlist(x)
  
  mat[,which(names(mat)=="dose")] <- x
  
  # exclude py after exitage
  if(length(which(mat$time >= mat[,3]))>0)
    mat <- mat[-which(mat$time >= mat[,3]),]
  
  # set a py ind within each person
  mat$id_ <- unlist(mat[,1])
  mat <- mat %>%
    dplyr::group_by(id_) %>%
    dplyr::mutate(n_pe=1:length(eval(id_)))
  mat <- mat[,-which(names(mat)=="id_")]
  
  # add the exit event as a row for each subject
  dt <- mat[which(mat$n_pe==1),]
  dt$time <- dt[,3]
  dt[,4]  <- outcome_
  dt$n_pe <- 0
  dt$dose <- 0
  
  attributes(dt) <- NULL
  dt <- as.data.frame(dt)
  names(dt) <- names(mat)
  
  attributes(mat) <- NULL
  mat <- as.data.frame(mat)
  names(mat) <- names(dt)
  
  # the counter n_pe goes 1:number of years, and 0 is the relative to exitage 1 is the row of entryage
  mat <- rbind(mat,dt)
  
  # keep only informative rows, so remove the 0 dose rows
  rows_to_remove <- which(mat$dose==0 & mat$n_pe>1)
  if(length(rows_to_remove)>0)
    mat <- mat[-rows_to_remove,]
  
  # set a py ind within each person
  dt0 <- mat[which(mat$n_pe==0),]
  mat <- mat[which(mat$n_pe>0),]
  
  mat$id_ <- unlist(mat[,1])
  mat <- mat %>%
    dplyr::group_by(id_) %>%
    dplyr::mutate(n_pe=1:length(eval(id_)))
  mat <- mat[,-which(names(mat)=="id_")]
  mat <- rbind(mat,dt0)
  
  # arrange rows
  mat$id_ <- unlist(mat[,1])
  mat <- dplyr::arrange(mat,id_,time)
  
  # add the cumulative exposure
  mat <- mat %>%
    dplyr::group_by(id_) %>%
    dplyr::mutate(dose_cum=cumsum(dose))
  mat <- mat[,-which(names(mat)=="id_")]
  
  a <- plyr::count(mat[,1])
  mat$id1 <- unlist(lapply(seq_along(1:n_sub),function(x) rep(x,each=a$freq[x])))
  
  # remove innecessary attributes
  attributes(mat) <- NULL
  mat <- as.data.frame(mat)
  names(mat) <- names(dt)
  
  return(mat)
}