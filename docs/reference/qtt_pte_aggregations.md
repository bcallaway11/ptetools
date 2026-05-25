# Aggregate Group-Time Quantile Treatment Effects

Aggregate group-time F0/F1 distributions into QTT curves at the overall,
group, and dynamic level. CDFs are mixed first using
[`BMisc::combine_ecdfs`](https://bcallaway11.github.io/BMisc/reference/combine_ecdfs.html)
and then inverted at all quantile levels in `probs`, avoiding the bias
from averaging scalar QTTs.

## Usage

``` r
qtt_pte_aggregations(attgt.list, ptep, extra_gt_returns)
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

named list with elements `overall_results`, `dyn_results`,
`group_results`, `F0_overall`, `F1_overall`
