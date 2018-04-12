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
* Wide format data set (wf) - where each row contain all the follow-up information of a subject, icluding times of exposures and doses      of exposures

### Using an event format data set as input cohort

load library
```
> library(rERR)
```
look at the data set
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

 set the formulas for two models
 ```
> formula1  <- Surv(entry_age,exit_age,outcome) ~ lin(dose_cum) + strata(sex)
> formula2  <- Surv(entry_age,exit_age,outcome) ~ loglin(factor(country)) + lin(dose_cum) + strata(sex)
```

fit the models
```
> fit1 <- f_fit_linERR_ef(formula1,data=cohort_ef,id_name="id",dose_name="dose",time_name="age",covars_names=c("sex"),lag=2,exclusion_done=T)
> fit2 <- f_fit_linERR_ef(formula2,data=cohort_ef,id_name="id",dose_name="dose",time_name="age",covars_names=c("sex","country"),lag=2,exclusion_done=T)
```

Summary of fit1
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

Summary of fit2
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

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
