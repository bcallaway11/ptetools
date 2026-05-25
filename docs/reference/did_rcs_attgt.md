# Repeated Cross Sections Difference-in-Differences for ATT(g,t)

Takes a local repeated cross sections data set and computes an estimate
of a group-time average treatment effect and corresponding influence
function using a repeated cross sections DID approach.

## Usage

``` r
did_rcs_attgt(gt_data, xformula = ~1, est_method = "dr", ...)
```

## Arguments

- gt_data:

  data that is "local" to a particular group-time average treatment
  effect

- xformula:

  one-sided formula for covariates used in the propensity score and
  outcome regression models

- est_method:

  Which type of estimation method to use. Default is "dr" for doubly
  robust. The other option is "reg" for regression adjustment.

- ...:

  extra function arguments; not used here

## Value

attgt_if
