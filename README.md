# rERR package

fit Excess Relative Risk model

## Introduction

In radiation epidemiology, ERR models are used to analyze dose-response relationships for event rate data.

Usual approaches to the analysis of cohort and case control data often follow from risk-set sampling designs, where at each failure 
time a new risk set is defined, including the index case and all the controls that were at risk at that time. That kind of sampling designs are usually related to the 
Cox proportional hazards model, available in most standard statistical packages but limited to log-linear model, of the form

![](http://mathurl.com/y7gp2qz5.png)

where ![](http://mathurl.com/y7ekq4k8.png) is the vector of explanatory variables.

One model of particular interest, especially in radiation environmental and occupational epidemiology is the linear ERR model, 

![](http://mathurl.com/y9qdc2mf.png)

where ![](http://mathurl.com/y7ekq4k8.png) are the covariates

Estimation of a dose-response trend under a linear relative rate model implies that for every 1-unit increase in the exposure metric, 
the rate of disease increases (or decreases) in an additive fashion. 

The modification of the effect of exposure in linear relative rate models by a study covariate z can be assessed by including a log-
linear subterm for the linear exposure, implying a model of the form

![](http://mathurl.com/y8qtoa7r.png)

## Installing

The package can be installed directly from github:

```
install.packages("devtools")
devtools::install_github("fbr600/rERR")
```

## Model Specification

* The ERR model is specified using a formula object in R

  ```
  formula <- Surv(entry_time,exit_time,outcome) ~ lin(dose_cum)
  ```
  This is the formula for the model:

  ![](http://mathurl.com/y7frgjzt.png)

  where f(dose) is the cumulative is the cumulative dose of each subject up to the relative fail time.

* Also the covariates effect can be included in the model:
  ```
  formula <- Surv(entry_time,exit_time,outcome) ~ loglin(country) + lin(dose_cum)
  ```
  This is the formula of the model:

  ![](http://mathurl.com/y95pxo7t.png)

  where ![](http://mathurl.com/y7ekq4k8.png) are the different countries.

* Stratification in the risksets is done by using the ``` strata()``` function:
  ```
  formula <- Surv(entry_time,exit_time,outcome) ~ loglin(country) + lin(dose_cum) + strata(sex) 
  ```
  Is the same model as before, but in the risksets only the subjects of the same sex as the case are taken as in-risk-subjects.

## Examples
Two different formats of data sets are allowed: 
* Event format data set (ef) - where each row represents an exposure event.
* Wide format data set (wf) - where each row contain all the follow-up information of a subject, icluding times and doses of exposures

### Using an event format data set as input cohort

##### load library
```
> library(rERR)
```
##### look at the data set
```
> head(cohort_ef,row.names=F)
 id sex entry_age exit_age outcome  age     dose country
 10   2      9.88 12.53114       0 7.88 15.80825      Be
 14   1     11.09 12.35592       0 9.09 13.96742      UK
 14   1     11.09 12.35592       0 9.17 13.81423      UK
 15   1      9.55 12.33402       0 7.55  9.25016      Fr
 16   1      8.48 12.29843       0 6.49 18.00776      Sp
 16   1      8.48 12.29843       0 9.16 11.35068      Sp
 ```

 #### set the formulas for two models
 ```
> formula1  <- Surv(entry_age,exit_age,outcome) ~ lin(dose_cum) + strata(sex)
> formula2  <- Surv(entry_age,exit_age,outcome) ~ loglin(factor(country)) + lin(dose_cum) + strata(sex)
```

##### fit the models
```
> fit1 <- f_fit_linERR_ef(formula1,data=cohort_ef,id_name="id",dose_name="dose",time_name="age",covars_names=c("sex"),lag=2,exclusion_done=T)
> fit2 <- f_fit_linERR_ef(formula2,data=cohort_ef,id_name="id",dose_name="dose",time_name="age",covars_names=c("sex","country"),lag=2,exclusion_done=T)
```

##### summary of fit1
```
> summary(fit1)
Formula:
Surv(entry_age, exit_age, outcome) ~ lin(dose_cum) + strata(sex)

Linear Parameter Summary Table:
               coef   se(coef)         z  Pr(>|z|)
dose_cum 0.02401129 0.04400845 0.5456063 0.5853366

AIC:  313.8759 
Deviance:  311.8759 
Number of risk sets:  18 
```

##### summary of fit2
```
> summary(fit2)
Formula:
Surv(entry_age, exit_age, outcome) ~ loglin(factor(country)) + 
    lin(dose_cum) + strata(sex)

Linear Parameter Summary Table:
               coef   se(coef)         z  Pr(>|z|)
dose_cum 0.02181835 0.04152847 0.5253829 0.5993171

Log Linear Parameter Summary Table:
                 coef exp(coef)  se(coef)          z  Pr(>|z|)
country_Fr 0.03444314  1.035043 1.0003142 0.03443232 0.9725324
country_Ge 0.70025564  2.014268 0.8660814 0.80853332 0.4187836
country_It 0.70146822  2.016712 0.8661484 0.80987072 0.4180145
country_Sp 0.04522023  1.046258 1.0001470 0.04521359 0.9639371
country_UK 0.69520396  2.004118 0.8661149 0.80266945 0.4221658

AIC:  321.9591 
Deviance:  309.9591 
Number of risk sets:  18
```

#### confidence intervals for fit1 parameters
```
> confint(fit1)
Confidence intervals:

Linear Parameter - Likelihood ratio test ci:
               coef   lower .95 upper .95
dose_cum 0.02401129 -0.01109805 0.8468688
```

##### confidence intervals for fit2 parameters
```
> confint(fit2)
Confidence intervals:

Linear Parameter - Likelihood ratio test ci:
               coef   lower .95 upper .95
dose_cum 0.02181835 -0.01138425  0.774703

Log Linear Parameter - Wald ci:
                 coef exp(coef) lower .95 upper .95
country_Fr 0.03444314  1.035043 0.1457101  7.352371
country_Ge 0.70025564  2.014268 0.3688989 10.998335
country_It 0.70146822  2.016712 0.3692980 11.013126
country_Sp 0.04522023  1.046258 0.1473371  7.429602
country_UK 0.69520396  2.004118 0.3670159 10.943634
```

##### likelihood ratio test between nested and nesting models
```
> f_lrt(fit1,fit2)
$lrt
[1] 1.916819

$lrt_pval
[1] 0.3835023
```
### Using an wide format data set as input cohort
##### load library
```
> library(rERR)
```
##### look at the data set
```
> head(cohort_wf)
id sex YearInit AgeAtEntry age_at_event outcome end_status ses number_of_ct    ctage1 ctage2 ... ctage35 dose1 dose2 ... dose35 country
 1   1     1983   7.957093    18.957093       0          2   1            1  7.957093      0           0  10.5     0          0      It
 2   1     1988  13.857040    18.857040       0          2   1            2 13.857040      0           0  10.4     0          0      Sp
 4   1     2008  16.496089    19.496089       0          2   1            1 16.496089      0           0   3.8     0          0      It
12   1     2004   8.134463    17.134463       0          2   3            2  8.134463      0           0   7.0     0          0      Sp
13   1     2009   2.554935     6.554935       0          2   2            2  2.554935      0           0   3.6     0          0      UK
16   1     1996  16.968197    18.968197       0          2   3            1 16.968197      0           0   3.5     0          0      Fr
```

##### set the formulas for the models
```
> formula1  <- Surv(AgeAtEntry,age_at_event,outcome) ~ lin(dose_cum) + strata(sex)
> formula2  <- Surv(AgeAtEntry,age_at_event,outcome) ~ loglin(factor(country)) + lin(dose_cum) + strata(sex)
```

##### fit the models
```
> fit1 <- f_fit_linERR_wf(formula1,data=cohort_wf,id_name="id",doses=cohort_wf[,45:79],times=cohort_wf[,10:44],
                        covars=cohort_wf[,c("sex","country")],lag=2,exclusion_done = F)

> fit2 <- f_fit_linERR_wf(formula2,data=cohort_wf,id_name="id",doses=cohort_wf[,45:79],times=cohort_wf[,10:44],
                        covars=cohort_wf[,c("sex","country")],lag=2,exclusion_done = F)
```

##### summary of fit1
```
> summary(fit1)
Formula:
Surv(AgeAtEntry, age_at_event, outcome) ~ lin(dose_cum) + strata(sex)

Linear Parameter Summary Table:
               coef   se(coef)        z  Pr(>|z|)
dose_cum 0.03882416 0.03977917 0.975992 0.3290684

AIC:  599.129 
Deviance:  597.129 
Number of risk sets:  34
```
##### summary of fit2
```
> summary(fit2)
Formula:
Surv(AgeAtEntry, age_at_event, outcome) ~ loglin(factor(country)) + 
    lin(dose_cum) + strata(sex)

Linear Parameter Summary Table:
              coef   se(coef)         z  Pr(>|z|)
dose_cum 0.0402001 0.04089224 0.9830742 0.3255709

Log Linear Parameter Summary Table:
                    coef    exp(coef)    se(coef)           z  Pr(>|z|)
country_Fr -14.827568523 3.634702e-07 616.7428233 -0.02404174 0.9808193
country_Ge  -0.005785829 9.942309e-01   0.5345654 -0.01082343 0.9913643
country_It   0.312718515 1.367137e+00   0.4929068  0.63443745 0.5257954
country_Sp  -0.606014765 5.455206e-01   0.6268803 -0.96671535 0.3336864
country_UK  -0.159932110 8.522016e-01   0.5565602 -0.28735816 0.7738381

AIC:  593.5899 
Deviance:  581.5899 
Number of risk sets:  34 
```

#### confidence intervals for fit1 parameters
```
> confint(fit1)
Confidence intervals:

Linear Parameter - Likelihood ratio test ci:
               coef    lower .95 upper .95
dose_cum 0.03882416 -0.003425317  0.278199
```

#### confidence intervals for fit2 parameters
```
> confint(fit2)
Confidence intervals:

Linear Parameter - Likelihood ratio test ci:
              coef    lower .95 upper .95
dose_cum 0.0402001 -0.003126192 0.2904963

Log Linear Parameter - Wald ci:
                    coef    exp(coef) lower .95 upper .95
country_Fr -14.827568523 3.634702e-07 0.0000000       Inf
country_Ge  -0.005785829 9.942309e-01 0.3487100  2.834720
country_It   0.312718515 1.367137e+00 0.5202941  3.592319
country_Sp  -0.606014765 5.455206e-01 0.1596650  1.863857
country_UK  -0.159932110 8.522016e-01 0.2862843  2.536806
```

##### likelihood ratio test between nested and nesting models
```
> f_lrt(fit1,fit2)
$lrt
[1] 15.53904

$lrt_pval
[1] 0.0004224168
```
