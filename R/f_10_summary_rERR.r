#' summary of a ERR fit 
#' 
#' display summary of the parameter and statistics of the model
#' @param object an object of class rERR
#' @return a list with the summary elements
#' @examples \donotrun{ summary(fit)}
#' @export
summary.rERR <- function(object)
{
  n_lin    <- nrow(attr(object,"lrt_ci"))
  n_loglin <- ifelse(is.null(attr(object,"wald_ci")),0,nrow(attr(object,"wald_ci")))
  
  lin_coeff    <- attr(object,"lin_coef")
  loglin_coeff <- attr(object,"loglin_coef")
  formula      <- attr(object,"formula")
  risk_sets    <- attr(object,"desc")$desc$n_events
  deviance     <- 2*attr(object,"details")$value
  AIC          <- attr(object,"AIC")
  
  ans <- list(linear_coefficients=lin_coeff,
              loglinear_coefficients=loglin_coeff,
              formula=formula,
              num_risksets=risk_sets,
              deviance=deviance,
              AIC=AIC)
  class(ans) <- "summary.rERR"
  return(ans)
}