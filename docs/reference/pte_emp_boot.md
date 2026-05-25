# Class for Empirical Bootstrap Results

Class for holding `ptetools` empirical bootstrap results

## Usage

``` r
pte_emp_boot(
  attgt_results,
  overall_results,
  group_results,
  dyn_results,
  overall_weights = NULL,
  dyn_weights = NULL,
  group_weights = NULL,
  extra_gt_returns = NULL,
  ptep = NULL
)
```

## Arguments

- attgt_results:

  `data.frame` holding attgt results

- overall_results:

  `data.frame` holding overall results

- group_results:

  `data.frame` holding group results

- dyn_results:

  `data.frame` holding dynamic results

- overall_weights:

  vector containing weights on underlying ATT(g,t) for overall treatment
  effect parameter

- dyn_weights:

  list containing weights on underlying ATT(g,t) for each value of `e`
  corresponding to the dynamic treatment effect parameters.

- group_weights:

  list containing weights on underlying ATT(g,t) corresponding to
  deliver averaged group-specific treatment effects

- extra_gt_returns:

  A place to return anything extra from particular group-time average
  treatment effect calculations. For DID, this might be something like
  propensity score estimates, regressions of untreated potential
  outcomes on covariates. For ife, this could be something like the
  first step regression 2sls estimates. This argument is also
  potentially useful for debugging.

- ptep:

  `pte_params` object, stored for reference in the result.

## Value

a `pte_emp_boot` object
