# Aggregate Group-Time Quantile of the Treatment Effect

Aggregate group-time distribution of the treatment effect into overall,
group, and dynamic effects.

## Usage

``` r
qott_pte_aggregations(attgt.list, ptep, extra_gt_returns)
```

## Arguments

- attgt.list:

  list of attgt results from `compute.pte`

- ptep:

  `pte_params` object

- extra_gt_returns:

  A place to return anything extra from particular group-time average
  treatment effect calculations. For DID, this might be something like
  propensity score estimates, regressions of untreated potential
  outcomes on covariates. For ife, this could be something like the
  first step regression 2sls estimates. This argument is also
  potentially useful for debugging.

## Value

`pte_emp_boot` object
