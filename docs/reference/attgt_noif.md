# Class for (g,t)-Specific Results without Influence Function

Class for holding returns from group-time specific estimates in settings
when an influence function is not returned

## Usage

``` r
attgt_noif(attgt, extra_gt_returns = NULL)
```

## Arguments

- attgt:

  group-time average treatment effect

- extra_gt_returns:

  A place to return anything extra from particular group-time average
  treatment effect calculations. For DID, this might be something like
  propensity score estimates, regressions of untreated potential
  outcomes on covariates. For ife, this could be something like the
  first step regression 2sls estimates. This argument is also
  potentially useful for debugging.

## Value

an `attgt_noif` object
