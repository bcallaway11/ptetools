# Empirical Bootstrap for QTT Curves

Runs the empirical bootstrap for the full QTT curve case
(`gt_type = "qtt"`). Called automatically by `panel_empirical_bootstrap`
when `gt_type == "qtt"`.

## Usage

``` r
qtt_empirical_bootstrap(
  attgt.list,
  ptep,
  setup_pte_fun,
  subset_fun,
  attgt_fun,
  extra_gt_returns,
  aggregation_fun = NULL,
  ...
)
```

## Arguments

- attgt.list:

  list of attgt results from `compute.pte`

- ptep:

  `pte_params` object

- setup_pte_fun:

  This is a function that should take in `data`, `yname` (the name of
  the outcome variable in `data`), `gname` (the name of the group
  variable), `idname` (the name of the id variable), and possibly other
  arguments such as the significance level `alp`, the number of
  bootstrap iterations `biters`, and how many clusters for parallel
  computing in the bootstrap `cl`. The key thing that needs to be
  figured out in this function is which groups and time periods ATT(g,t)
  should be computed in. The function should return a `pte_params`
  object which contains all of the parameters passed into the function
  as well as `glist` and `tlist` which should be ordered lists of groups
  and time periods for ATT(g,t) to be computed.

  This function provides also provides a good place for error handling
  related to the types of data that can be handled.

  The `pte` package contains the function `setup_pte` that is a
  lightweight function that basically just takes the data, omits the
  never-treated group from `glist` but includes all other groups and
  drops the first time period. This works in cases where ATT would be
  identified in the 2x2 case (i.e., where there are two time periods, no
  units are treated in the first period and the identification strategy
  "works" with access to a treated and untreated group and untreated
  potential outcomes for both groups in the first period) — for example,
  this approach works if DID is the identification strategy.

- subset_fun:

  This is a function that should take in `data`, `g` (for group), `tp`
  (for time period), and `...` and be able to return the appropriate
  `data.frame` that can be used by `attgt_fun` to produce ATT(g=g,t=tp).
  The data frame should be constructed using `gt_data_frame` in order to
  guarantee that it has the appropriate columns that identify which
  group an observation belongs to, etc.

- attgt_fun:

  This is a function that should work in the case where there is a
  single group and the "right" number of time periods to recover an
  estimate of the ATT. For example, in the contest of difference in
  differences, it would need to work for a single group, find the
  appropriate comparison group (untreated units), find the right time
  periods (pre- and post-treatment), and then recover an estimate of ATT
  for that group. It will be called over and over separately by groups
  and by time periods to compute ATT(g,t)'s.

  The function needs to work in a very specific way. It should take in
  the arguments: `data`, `...`. `data` should be constructed using the
  function `gt_data_frame` which checks to make sure that `data` has the
  correct columns defined. `...` are additional arguments (such as
  formulas for covariates) that `attgt_fun` needs. From these arguments
  `attgt_fun` must return a list with element `ATT` containing the
  group-time average treatment effect for that group and that time
  period.

  If `attgt_fun` returns an influence function (which should be provided
  in a list element named `inf_func`), then the code will use the
  multiplier bootstrap to compute standard errors for group-time average
  treatment effects, an overall treatment effect parameter, and a
  dynamic treatment effect parameter (i.e., event study parameter). If
  `attgt_fun` does not return an influence function, then the same
  objects will be computed using the empirical bootstrap. This is
  usually (perhaps substantially) easier to code, but also will usually
  be (perhaps substantially) computationally slower.

- extra_gt_returns:

  A place to return anything extra from particular group-time average
  treatment effect calculations. For DID, this might be something like
  propensity score estimates, regressions of untreated potential
  outcomes on covariates. For ife, this could be something like the
  first step regression 2sls estimates. This argument is also
  potentially useful for debugging.

- aggregation_fun:

  An optional function for aggregating group-time treatment effects.
  When `NULL` (the default), the function is selected automatically
  based on `gt_type`.

- ...:

  extra arguments that can be passed to create the correct subsets of
  the data (depending on `subset_fun`), to estimate group time average
  treatment effects (depending on `attgt_fun`), or to aggregating
  treatment effects (particularly useful are `min_e`, `max_e`, and
  `balance_e` arguments to event study aggregations)

## Value

`pte_qtt` object
