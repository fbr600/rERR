#' likelihood ratio test
#'
#' function that thtat compute the lrt test for a nested and a nesting models
#' @param fit1 the nested model
#' @param fit2 the nesting model
#' @return a list containing the lrt statistic and the corresponding p_value from the Chi-square test
#' @examples \donotrun{ lrt(fit1,fit2)}
#' @importFrom stats pchisq
#' @export

f_lrt <- function(fit1,fit2)
{
  lrt1 <- 2*attr(fit1,"details")$value
  lrt2 <- 2*attr(fit2,"details")$value

  #### loglikelihood ratio test fit3 and fit4 ####
  lrt      <- lrt1-lrt2
  lrt_pval <- pchisq(lrt, df=2, ncp = 0, lower.tail = FALSE, log.p = FALSE)

  return(list(lrt=lrt,lrt_pval=lrt_pval))

}
