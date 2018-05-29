#' Simulated cohort of subjects exposed to ct scans
#'
#' List of scans recived by a cohort of subjects
#'
#' @format A data frame with 41744 rows and 8 variables:
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
#' @format A data frame with 10000 rows and 80 variables:
#' \describe{
#'   \item{id}{subject identificator, integer}
#'   \item{sex}{sex of the subject, 1-male / 2-female}
#'   \item{YearInit}{Year of entry of the subject, integer}
#'   \item{AgeAtEntry}{age of entry at the cohort, numeric}
#'   \item{age_at_event}{age at exit of the cohort, numeric}
#'   \item{outcome}{disease or not, 0-no disease / 1-disease}
#'   \item{end_status}{status at exit of the cohort, 0-disease / 1-death / 2-healthy}
#'   \item{ses}{socio economic status,integer}
#'   \item{number_of_ct}{number of ct's recived in all follow-up, integer}
#'   \item{ctage1}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage2}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage3}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage4}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage5}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage6}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage7}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage8}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage9}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage10}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage11}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage12}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage13}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage14}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage15}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage16}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage17}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage18}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage19}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage20}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage21}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage22}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage23}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage24}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage25}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage26}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage27}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage28}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage29}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage30}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage31}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage32}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage33}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage34}{middle age at i-th year of being in the cohort, numeric}
#'   \item{ctage35}{middle age at i-th year of being in the cohort, numeric}
#'	 \item{dose1}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose2}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose3}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose4}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose5}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose6}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose7}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose8}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose9}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose10}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose11}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose12}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose13}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose14}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose15}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose16}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose17}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose18}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose19}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose20}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose21}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose22}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose23}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose24}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose25}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose26}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose27}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose28}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose29}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose30}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose31}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose32}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose33}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose34}{grouped doses recived the i-th year of being in the cohort, numeric}
#'	 \item{dose35}{grouped doses recived the i-th year of being in the cohort, numeric}
#'   \item{country}{country, character}
#' }
"cohort_wf"

