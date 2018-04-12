

# things to check:
#
#   context("Exclusion in wf")
#   - Number of subjects after exclusion is minor or equal
#   - there is no subject with less than lag years of followup
#
#   context("Exclusion in ef")
#   - Number of subjects after exclusion is minor or equal
#   - there is no subject with less than lag years of followup
#
#   context("f to event table ef")
#   - f to event table preserve the cases and controls
#   - every subject has two events at least: entry and exit
#   - number of subjects is equal
#
#   context("f to event table wf")
#   - f to event table preserve the cases and controls
#   - every subject has two events at least: entry and exit
#   - number of subjects is equal
#
#   context("risk sets")

context("Exclusion in wf")

formula <- Surv(AgeAtEntry,age_at_event,outcome) ~ lin(dose_cum)+strata(sex)
dt1     <- f_exclusion(formula,cohort_wf,lag=1)
dt2     <- f_exclusion(formula,cohort_wf,lag=2)

test_that("Number after exclusion is minor",
{
  n_sub_pre_exclusion   <- length(unique(cohort_wf$id))
  n_sub_aft_exclusion_1 <- length(unique(dt1$id))
  n_sub_aft_exclusion_2 <- length(unique(dt2$id))

  expect_lte(n_sub_aft_exclusion_2,n_sub_aft_exclusion_1)
  expect_lte(n_sub_aft_exclusion_1,n_sub_pre_exclusion)
})

test_that("follow up time is greater or equal than lag",
{
  # there is no subject with less than lag years of followup
  lag <- 2

  dt2$time_follow_up <- dt2$age_at_event - dt2$ctage1
  expect_equal(length(dt2$id[which(dt2$time_follow_up<lag)]),0)
})

context("Exclusion in ef")

formula <- Surv(entry_age,exit_age,outcome) ~ lin(dose_cum)+strata(sex)
dt1     <- f_exclusion(formula,cohort_ef,lag=1)
dt2     <- f_exclusion(formula,cohort_ef,lag=2)

test_that("Number after exclusion is minor",
{
  # do exclusions for 1 year and 2 years and check if the number reduces
  n_sub_pre_exclusion   <- length(unique(cohort_ef$id))
  n_sub_aft_exclusion_1 <- length(unique(dt1$id))
  n_sub_aft_exclusion_2 <- length(unique(dt2$id))

  expect_lte(n_sub_aft_exclusion_2,n_sub_aft_exclusion_1)
  expect_lte(n_sub_aft_exclusion_1,n_sub_pre_exclusion)
})
test_that("follow up time is greater or equal than lag",
{
  # there is no subject with less than lag years of followup
  lag <- 2

  dt2$time_follow_up <- dt2$exit_age - dt2$entry_age+lag
  expect_equal(length(dt2$id[which(dt2$time_follow_up<lag)]),0)
})

context("f to event table ef")

formula <- Surv(entry_age,exit_age,outcome) ~ lin(dose_cum)+strata(sex,country)
dt      <- f_to_event_table_ef_all(formula,data = cohort_ef,id_name = "id",dose_name = "dose",time_name = "age",covars_names = c("country","sex"))

test_that("Subjects are the same after the transformation",
{
  # same number of subjects at the end
  expect_equal( length(unique(dt$id)) , length(unique(cohort_ef$id)) )
  
  # the subjects are the same
  expect_equal( sort(unique(dt$id)) , sort(unique(cohort_ef$id)) )
})

test_that("the same number of outcomes in both data sets",
{ 
  aux <- distinct(cohort_ef[,c("id","outcome")])
  expect_equal( sum(aux$outcome) , sum(dt$outcome))
})

test_that("One and exactly one entry and exit events per person",
{
  # every body has 1 exit event
  expect_equal( dim(dt[which(dt$n_pe==0),])[1] , length(unique(cohort_ef$id)))
  # every body has 1 enter event
  expect_equal( dim(dt[which(dt$n_pe==1),])[1] , length(unique(cohort_ef$id)))
})


