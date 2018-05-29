#' plot the likelihood 
#' 
#' plot the partial log likelihood function in the case of one dimension in the linear part
#' @param object An rERR class object
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+\cr
#'                     lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data data set returned from f_to_model_data
#' @param rsets list of risksets, output of f_risksets
#' @param n_lin_vars number of linear variables (attribute of the to_model_data)
#' @param n_loglin_vars number of loglinear varaibles (attribute of the to_model_data)
#' @param id_name name of variable containing the names of subjects
#' @param time_name name of the time variable
#' @return rERR object with the estimation
#' @examples \dontrun{f_fit_linERR(formula,data,rsets,n_lin_vars,n_loglin_vars,id_name,time_name)}
#' @import survival
#' @import stats4
#' @import ggplot2
#' @importFrom stats qchisq 
#' @importFrom stats uniroot 
#' @importFrom stats pnorm 
#' @importFrom stats qnorm 
#' @importFrom stats pchisq
#' @importFrom numDeriv hessian
#' @export
f_plot_linERR <- function(object,formula,data,rsets,n_lin_vars,n_loglin_vars,id_name,time_name)
{
  
  # to avoid the note in the package check
  log_Likelihood <- NULL
  
  # formula left side
  formula_sv <- formula[[2]]
  
  # resultant data.frame
  v_id      <- eval(parse(text=paste0("data$",id_name)))
  v_n_pe    <- eval(parse(text=paste0("data$","n_pe")))
  v_entry   <- eval(parse(text=paste0("data$",formula_sv[[2]])))
  v_exit    <- eval(parse(text=paste0("data$",formula_sv[[3]])))
  v_outcome <- eval(parse(text=paste0("data$",formula_sv[[4]])))
  v_time    <- eval(parse(text=paste0("data$",time_name)))
  
  nrow_cases <- which(v_outcome==1)
  failtimes  <- v_exit[nrow_cases]
  id_cases   <- v_id[nrow_cases]
  
  nrow_cases <- names(rsets)
  # data[, 1] n_row
  # data[, 2] id_name
  # data[, 3] n_pe
  # data[, 4] entry_name
  # data[, 5] exit_name
  # data[, 6] outcome
  # data[, 7] time
  # data[, 8:(8+n_lin_vars)] linear covariates (already categorized if required)
  # data[, (8+n_lin_vars+1):(8+n_lin_vars+1]n_loglinear_vars)] loglinear vars, also categorized when required 
  
  # rsets:
  #   rsets[[1]] list of n_row that have all the values for the risk set of the 1st failtime
  #   (...)
  #   rsets[[n]] list of n_row that have all the values for the risk set of the nth failtime
  
  # initial condition: 0.1 for all
  beta        <- rep(0.1,n_lin_vars+n_loglin_vars)
  beta        <- as.list(beta)
  names(beta) <- paste0("x",1:length(beta))
  
  # function to set the number of arguments to the likelihood function
  add.arguments <- function(f, n) 
  {
    t = paste("arg <- alist(", paste(sapply(1:n, function(i) paste("x", i, "=", sep = "")), collapse = ","), ")", sep = "")
    formals(f) <- eval(parse(text = t))
    f
  }
  
  # likelihood function
  p.est <- function()
  {
    beta3 <- vector()
    for (i in 1:length(beta)) 
    {
      beta3[i] <- eval(parse(text = paste0("x", i)))
    }
    beta_2     <- as.list(beta3)
    beta_2     <- unlist(lapply(beta_2, as.numeric))
    constr_ind <- as.integer(0)
    res        <- .Call("llhood_linear_v3",beta_2,length(beta_2),length(rsets),
                        rsets,nrseti,data,nrow_cases,n_lin_vars,n_loglin_vars,constr_ind)
    #cons <<- res[length(res)]
    res  <- res[-length(res)]
    res1 <- log(res)
    return(-sum(log(res)))
  }
  
  # set the number of variables to the likelihood function
  p.est <- add.arguments(p.est, length(beta))
  
  # proper formats for the likelihood
  data          <- lapply(data,as.numeric)
  rsets         <- lapply(rsets,as.numeric)
  nrseti        <- lapply(rsets,length)
  nrseti        <- lapply(nrseti,as.numeric)
  n_lin_vars    <- as.integer(n_lin_vars)
  n_loglin_vars <- as.integer(n_loglin_vars)
  nrow_cases    <- lapply(nrow_cases,as.numeric)
  
  # constraint for lin variables so llik > 0
  constr <- vector()
  if(n_lin_vars>0)
    for(i in 0:(n_lin_vars-1))
    {
      beta_2 <- unlist(lapply(beta,as.numeric))
      res    <- .Call("llhood_linear_v3",beta_2,length(beta_2),length(rsets),
                      rsets,nrseti,data,nrow_cases,n_lin_vars,n_loglin_vars,as.integer(i))
      constr[i+1] <- res[length(res)]
    }
  
  # not constraint for loglinear, cause of exponential function
  llim  <- c(constr,rep(-Inf, length(beta)-n_lin_vars))
  
  # ensure that the hessian is enterely computable at the contsrain
  llim2 <- c(constr,rep(.1, length(beta)-n_lin_vars))
  reduction <- 0.999
  p.est_num <- function(x)
  {
    x <- as.list(x)
    names(x) <- paste0("x",1:length(x))
    return(do.call(p.est,x))
  }
  suppressWarnings(
  while(is.na(numDeriv::hessian(p.est_num,llim2*reduction)))
    reduction <- reduction^2
  )
  
  # lrt confidence interval: only for linear varaibles
  if(n_lin_vars == 1)
  {
    beta_good <- attr(object,"coef")
    llik      <- attr(object,"details")$value
    prob      <- .95
    llim      <- constr*.999
    g <- function(x)
    {
      aux        <- beta_good
      aux        <- as.list(aux)
      aux[[1]]   <- x
      names(aux) <- paste0("x",1:length(aux))
      
      y          <- do.call(p.est,aux)
      
      return(-y )
    }
    beta_good <- attr(object,"coef")
    
    attr(object,"lrt_ci")[2]
    
    x <- seq(from=llim[1],to=attr(object,"lrt_ci")[2]*1.5,length.out = 100)
    y <- unlist(lapply(x,g))
    dt <- data.frame(x=x,log_Likelihood=y) 
    
    ggplot2::ggplot(dt,ggplot2::aes(x=x,y=log_Likelihood))+ggplot2::geom_line()+
      ggplot2::geom_vline(xintercept = attr(object,"fullcoef")[1],col="red")+
      ggplot2::geom_hline(yintercept = -attr(object,"details")$value-qchisq(.95,1)/2,col="red")+
      ggplot2::xlab("beta")+
      ggplot2::ylab("Partial Log Likelihood")+
      ggplot2::ggtitle(paste0("Maximum Likelihood Estimate: beta = ",sprintf("%.4f",beta_good),"\n95% Lrt ci: ( ",
                              sprintf("%.3f",attr(object,"lrt_ci")[1])," , ",sprintf("%.3f",attr(object,"lrt_ci")[2])," )"))
    
  }
  
  
}
