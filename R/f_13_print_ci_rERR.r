#' Print ci rERR 
#' 
#' Print the cofnidence intervals of rERR fit
#' @param x a ci.ERR object
#' @export
print.ci.rERR <- function (x, ...) 
{
  cat("Confidence intervals:\n")
  
  if(!is.null(x$linear_ci))
  {
    cat("\nLinear Parameter - Likelihood ratio test ci:\n")
    print(x$linear_ci)
  }
  if(!is.null(x$loglinear_ci))
  {
    cat("\nLog Linear Parameter - Wald ci:\n")
    print(x$loglinear_ci)
  }
}