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

The ERR model is specified using a formula object in R

```
formula <- Surv(entry_time,exit_time,outcome) ~ lin(dose_cum)
```
This is the formula for the model:

![](http://mathurl.com/y7frgjzt.png)

where f(dose) is the cumulative is the cumulative dose of each subject up to the relative fail time.

Also the covariates effect can be included in the model:
```
formula <- Surv(entry_time,exit_time,outcome) ~ loglin(country) + lin(dose_cum)
```
This is the formula of the model:

![](http://mathurl.com/y95pxo7t.png)

where ![](http://mathurl.com/y7ekq4k8.png) are the different countries.

Stratification in the risksets is done by using the ``` strata()``` function:
```
formula <- Surv(entry_time,exit_time,outcome) ~ loglin(country) + lin(dose_cum)+strata(sex)
```
Is the same model as before, but in the risksets only the subjects of the same sex as the case are taken as in-risk-subjects.

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
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
