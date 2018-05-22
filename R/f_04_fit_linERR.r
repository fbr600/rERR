#' fit Excess Relative Risk Model (internal use)
#' 
#' function that calls the optimization (mle from stats4 package, so use optim), and return a rERR object with the estimation and summary
#' @param formula Surv(entry_time,exit_time,outcome)~loglin(loglin_var1,..,loglin_varn)+lin(lin_var1,..,lin_varm)+strata(strat_var1,...strat_varp)
#' @param data data set returned from f_to_model_data
#' @param rsets list of risksets, output of f_risksets
#' @param n_lin_vars number of linear variables (attribute of the to_model_data)
#' @param n_loglin_vars number of loglinear varaibles (attribute of the to_model_data)
#' @param id_name name of variable containing the names of subjects
#' @param time_name name of the time variable
#' @return rERR object with the estimation
#' @examples \donotrun{ f_fit_linERR(formula,data,rsets,n_lin_vars,n_loglin_vars,id_name,time_name)}
#' @import survival
#' @import stats4
#' @importFrom stats qchisq 
#' @importFrom stats uniroot 
#' @importFrom stats pnorm 
#' @importFrom stats qnorm 
#' @importFrom stats pchisq
#' @export
f_fit_linERR <- function(formula,data,rsets,n_lin_vars,n_loglin_vars,id_name,time_name)
{
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
    cons <<- res[length(res)]
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
  for(i in 0:(n_lin_vars-1))
  {
    beta_2 <- unlist(lapply(beta,as.numeric))
    res    <- .Call("llhood_linear_v3",beta_2,length(beta_2),length(rsets),
                    rsets,nrseti,data,nrow_cases,n_lin_vars,n_loglin_vars,as.integer(i))
    constr[i+1] <- res[length(res)]
  }
  
  # not constraint for loglinear, cause of exponential function
  llim  <- c(constr,rep(-Inf, length(beta)-n_lin_vars))
  
  # in case of multivariate linear it may happen that even with the component constraint, the joint is not finite
  reduction <- 0.999
  if(n_lin_vars>0 & n_loglin_vars==0)
    while(!is.finite(do.call(p.est,as.list(llim*reduction))))
      reduction <- reduction^2
  
  # optimization
  res <- mle(minuslogl = p.est,start = beta,method = "L-BFGS-B",lower=llim*reduction)
   
  # res <- bbmle::mle2(minuslogl = p.est,start = beta,method = "L-BFGS-B",lower=llim*reduction)
  
  # summary
  names                       <- names(data)[8:(8+n_lin_vars+n_loglin_vars-1)]
  names(attr(res,"coef"))     <- names
  names(attr(res,"fullcoef")) <- names
  attr(res,"desc")            <- list(desc=list(n_events=length(rsets),
                                      n_observations=length(data$n_pe[which(data$n_pe!=0)]),
                                      n_subjects=length(data$n_pe[which(data$n_pe==1)])))
  
  # lrt confidence interval: only for linear varaibles
  if(n_lin_vars > 0)
  {
    lrt_ci_mat           <- matrix(nrow=n_lin_vars,ncol=2)
    rownames(lrt_ci_mat) <- names[1:n_lin_vars]
    colnames(lrt_ci_mat) <- c("lower .95","upper .95")
    
    for(i in 1:n_lin_vars)
    {
      attr(res,"details")
      
      beta_good <- attr(res,"coef")
      llik      <- attr(res,"details")$value
      prob      <- .95
      llim      <- constr*.999
      g <- function(x)
      {
        aux        <- beta_good
        aux        <- as.list(aux)
        aux[[i]]   <- x
        names(aux) <- paste0("x",1:length(aux))
        
        y          <- do.call(p.est,aux)
        
        return(-y + llik + qchisq(prob, 1)/2)
      }
      # find interval as 0 of g function g(beta) = minulsloglik(beta) + minusloglik(beta_est) +  qchisq(prob, 1)/2)
      l1_t <- try(uniroot(g,lower=llim[i],upper=beta_good[i],extendInt="no")$root)
      l2_t <- try(uniroot(g,lower=beta_good[i],upper=100000, extendInt = "yes")$root)
      if(is.numeric(l1_t))
        l1 <- l1_t
      else
        l1 <- -Inf
      if(is.numeric(l2_t))
        l2 <- l2_t
      else
        l2 <- Inf
      #print(c(l1,l2))
      if(beta_good[i]>l2)
        l2 <- Inf
      lrt_ci_mat[i,1] <- l1
      lrt_ci_mat[i,2] <- l2
    }
    attr(res,"lrt_ci") <- lrt_ci_mat
    sum                <- summary(res)
    coef               <- attr(sum,"coef")[1:n_lin_vars,1]
    se                 <- attr(sum,"coef")[1:n_lin_vars,2]
    
    coef_mat           <- matrix(nrow=n_lin_vars,ncol=4)
    rownames(coef_mat) <- names[1:n_lin_vars]
    colnames(coef_mat) <- c("coef","se(coef)","z","Pr(>|z|)")
    coef_mat[,1]       <- coef
    coef_mat[,2]       <- se
    coef_mat[,3]       <- coef_mat[,1]/coef_mat[,2]
    coef_mat[,4]       <- 2*(1-pnorm(abs(coef_mat[,3])))
    
    attr(res,"lin_coef") <- coef_mat
  }
  
  # wald ci for loglinear variables
  if(n_loglin_vars>0)
  {
    sum <- summary(res)
    coef <- attr(sum,"coef")[(n_lin_vars+1):(n_lin_vars+n_loglin_vars),1]
    se   <- attr(sum,"coef")[(n_lin_vars+1):(n_lin_vars+n_loglin_vars),2]
    
    coef_mat           <- matrix(nrow=n_loglin_vars,ncol=5)
    rownames(coef_mat) <- names[(n_lin_vars+1):(n_lin_vars+n_loglin_vars)]
    colnames(coef_mat) <- c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")
    
    coef_mat[,1] <- coef
    coef_mat[,2] <- exp(coef)
    coef_mat[,3] <- se
    coef_mat[,4] <- coef_mat[,1]/coef_mat[,3]
    coef_mat[,5] <- 2*(1-pnorm(abs(coef_mat[,4])))
    
    conf_mat           <- matrix(nrow=n_loglin_vars,ncol=4)
    colnames(conf_mat) <- c("exp(coef)","exp(-coef)","lower .95","upper .95")
    rownames(conf_mat) <- names[(n_lin_vars+1):(n_lin_vars+n_loglin_vars)]
    conf_mat[,1]       <- exp(coef)
    conf_mat[,2]       <- exp(-coef)
    conf_mat[,3]       <- exp(coef_mat[,1]-qnorm(0.975,mean=0,sd=1)*se)
    conf_mat[,4]       <- exp(coef_mat[,1]+qnorm(0.975,mean=0,sd=1)*se)
    
    attr(res,"wald_ci")     <- conf_mat
    attr(res,"loglin_coef") <- coef_mat
  }
  
  # lrt: against null
  lrt_stat <- 2 *(-attr(res,"details")$value - sum(log(1/unlist(lapply(rsets,length)))))
  lrt_pval <- 1 - pchisq(lrt_stat,1,lower.tail = T)
  
  attr(res,"formula") <- formula
  attr(res,"lrt")     <- list(lrt_stat=lrt_stat,lrt_pval=lrt_pval)
  attr(res,"se")      <- attr(summary(res),"coef")[,2]
  attr(res,"AIC")     <- AIC(res)
  class(res)          <- "rERR"
  return(res)
}
