#' Simulated cohort of subjects exposed to ct scans
#'
#' List of scans recived by a cohort of subjects
#'
#' @format A data frame with 93017 rows and 8 variables:
#' \describe{
#'   \item{id}{subject identificator, integer}
#'   \item{sex}{sex of the subject, 1-male / 2-female}
#'   \item{entry_age}{age of entry at the cohort, numeric}
#'   \item{exit_age}{age at exit of the cohort, numeric}
#'   \item{outcome}{disease or not, 0-no disease / 1-disease}
#'   \item{age}{age at scan, numeric}
#'   \item{dose}{dose of the scan, numeric in mGy}
#'   \item{country}{country, character}
#' }
"cohort_ef"


#' Simulated cohort of subjects exposed to ct scans
#'
#' List of subjects that recived at least 1 scan
#'
#' @format A data frame with 93017 rows and 8 variables:
#' \describe{
#'   \item{id}{subject identificator, integer}
#'   \item{sex}{sex of the subject, 1-male / 2-female}
#'   \item{AgeAtEntry}{age of entry at the cohort, numeric}
#'   \item{age_at_event}{age at exit of the cohort, numeric}
#'   \item{outcome}{disease or not, 0-no disease / 1-disease}
#'   \item{end_status}{status at exit of the cohort, 0-disease / 1-death / 2-healthy}
#'   \item{number_of_ct}{number of ct's recived in all follow-up, integer}
#'   \item{ctage_i}{middle age at i-th year of being in the cohort, numeric}
#'   \item{dose_i}{grouped doses recived the i-th year of being in the cohort, numeric}
#'   \item{country}{country, character}
#' }
"cohort_wf"

