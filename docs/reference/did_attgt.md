# Difference-in-differences for ATT(g,t)

Takes a data.frame and computes for a particular group g and time period
t and computes an estimate of a group time average treatment effect and
a corresponding influence function using a difference in differences
approach.

The code relies on `gt_data` having certain variables defined. In
particular, there should be an `id` column (individual identifier), `D`
(treated group identifier), `period` (time period), `name` (equal to
"pre" for pre-treatment periods and equal to "post" for post treatment
periods), `Y` (outcome).

In our case, we call `two_by_two_subset` which sets up the data to have
this format before the call to `did_attgt`.

## Usage

``` r
did_attgt(gt_data, xformula = ~1, ...)
```

## Arguments

- gt_data:

  data that is "local" to a particular group-time average treatment
  effect

- xformula:

  one-sided formula for covariates used in the propensity score and
  outcome regression models

- ...:

  extra function arguments; not used here

## Value

attgt_if
