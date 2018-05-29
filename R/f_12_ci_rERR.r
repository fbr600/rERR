#' Confidence intervals 
#' 
#' Show the confidence intervals for each parameter of the model.
#' The likelihood ratio test ci for linear varaibles, and the Wald ci for the loglinear terms
#' @param object an object of class rERR
#' @param \ldots for future methods
#' @return a list with the confidence intervals
#' @examples \dontrun{ci.rERR(fit)}
#' @export
confint.rERR <- function(object, ...)
{
  n_lin    <- ifelse(is.null(attr(object,"lrt_ci")),0,nrow(attr(object,"lrt_ci")))
  n_loglin <- ifelse(is.null(attr(object,"wald_ci")),0,nrow(attr(object,"wald_ci")))
  
  lin_coeff    <- attr(object,"lin_coef")
  lin_ci       <- attr(object,"lrt_ci")
  loglin_coeff <- attr(object,"loglin_coef")
  loglin_ci    <- attr(object,"wald_ci")
  
  lin_ci              <- cbind(lin_coeff[,1],lin_ci)
  
  if(n_lin>0)
    colnames(lin_ci)[1] <- "coef"
  
  loglin_ci    <- cbind(loglin_coeff[,c(1,2)],loglin_ci[,c(3,4)])
  
  ans          <- list(linear_ci=lin_ci,
                       loglinear_ci=loglin_ci)
  class(ans) <- "ci.rERR"
  return(ans)
}
