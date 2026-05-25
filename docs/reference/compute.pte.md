# Heavy-Lifting for pte Function

Function that actually computes panel treatment effects. The difference
relative to `compute.pte` is that this function loops over time periods
first (instead of groups) and tries to estimate model for untreated
potential outcomes jointly for all groups.

## Usage

``` r
compute.pte(ptep, subset_fun, attgt_fun, ...)
```

## Arguments

- ptep:

  `pte_params` object

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

- ...:

  extra arguments that can be passed to create the correct subsets of
  the data (depending on `subset_fun`), to estimate group time average
  treatment effects (depending on `attgt_fun`), or to aggregating
  treatment effects (particularly useful are `min_e`, `max_e`, and
  `balance_e` arguments to event study aggregations)

## Value

a list containing the following elements:

- `attgt.list`: list of ATT(g,t) estimates

- `inffunc`: influence function matrix

- `extra_gt_returns`: list of extra returns from gt-specific
  calculationsons
