# Class for (g,t)-Specific Results with Influence Function

Class for holding group-time average treatment effects along with their
influence function

## Usage

``` r
attgt_if(attgt, inf_func, extra_gt_returns = NULL)
```

## Arguments

- attgt:

  group-time average treatment effect

- inf_func:

  influence function

- extra_gt_returns:

  A place to return anything extra from particular group-time average
  treatment effect calculations. For DID, this might be something like
  propensity score estimates, regressions of untreated potential
  outcomes on covariates. For ife, this could be something like the
  first step regression 2sls estimates. This argument is also
  potentially useful for debugging.

## Value

`attgt_if` object
