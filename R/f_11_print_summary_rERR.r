#' Print summary rERR 
#' 
#' Print the summary of rERR fit
#' @param x a summary.ERR object
#' @export
print.summary.rERR <- function (x, ...) 
{
  cat("Formula:\n")
  print(x$formula)
  if(!is.null(x$linear_coefficients))
  {
    cat("\nLinear Parameter Summary Table:\n")
    print(x$linear_coefficients)
  }
  if(!is.null(x$loglinear_coefficients))
  {
    cat("\nLog Linear Parameter Summary Table:\n")
    print(x$loglinear_coefficients)
  }
  cat("\nAIC: ", x$AIC, "\n")
  cat("Deviance: ", x$deviance, "\n")
  cat("Number of risk sets: ", x$num_risksets, "\n")
}
