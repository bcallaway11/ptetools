# Generic Setup Function

This is a function for how to setup the data to be used in the
`ptetools` package.

The `setup_pte` function builds on `setup_pte_basic` and attempts to
provide a general purpose function (with error handling) to arrange the
data in a way that can be processed by `subset_fun` and `attgt_fun` in
the next steps.

## Usage

``` r
setup_pte(
  yname,
  gname,
  tname,
  idname = NULL,
  data,
  panel = TRUE,
  required_pre_periods = 1,
  anticipation = 0,
  base_period = "varying",
  control_group = "notyettreated",
  clustervars = NULL,
  require_comparison = TRUE,
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
  bstrap = TRUE,
  weightsname = NULL,
  gt_type = "att",
  ret_quantile = 0.5,
  probs = NULL,
  biters = 100,
  cl = 1,
  call = NULL,
  ...
)
```

## Arguments

- yname:

  Name of outcome in `data`

- gname:

  Name of group in `data`

- tname:

  Name of time period in `data`

- idname:

  Name of id in `data`

- data:

  balanced panel or repeated cross sections data

- panel:

  Whether the data are panel data. The default is TRUE. Set to FALSE for
  repeated cross sections.

- required_pre_periods:

  The number of required pre-treatment periods to implement the
  estimation strategy. Default is 1.

- anticipation:

  how long before the treatment actually takes place it can already
  affect outcomes, in units of the time variable `tname` (e.g., years),
  as in
  [`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html).
  With unequally spaced periods this is not the same as a number of
  periods. Units first treated more than `anticipation` after the last
  period are treated as never treated.

- base_period:

  The type of base period to use. This only affects the numeric value of
  results in pre-treatment periods. Results in post-treatment periods
  are not affected by this choice. The default is "varying", where the
  base period will "back up" to the immediately preceding period in
  pre-treatment periods. The other option is "universal" where the base
  period is fixed in pre-treatment periods to be the period right before
  the treatment starts. "Universal" is commonly used in
  difference-in-differences applications, but can be unnatural for other
  identification strategies.

- control_group:

  Which group is used as the comparison group. The default choice is
  "notyettreated" (units not yet treated in either period of a
  group-time comparison, including never-treated units). "nevertreated"
  uses only never-treated units, and "eventuallytreated" uses
  not-yet-treated units excluding the never treated (as in
  `setup_pte()`; units first treated after the last period count as
  eventually treated). Different estimation strategies can implement
  their own choices for the control group.

- clustervars:

  The name of a variable to cluster standard errors on, in addition to
  `idname` (as in
  [`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html)):
  one variable, constant over time within each unit (e.g. state when
  units are counties). The default is NULL, which clusters at the unit
  level. Clustering applies to analytical standard errors, the
  multiplier bootstrap (one multiplier per cluster) and the empirical
  bootstrap (whole clusters are resampled).

- require_comparison:

  Whether the design must have comparison units (default `TRUE`).
  `FALSE` is for estimators that use only variation within each group
  and build their own group-time cells without comparison units, such as
  contdid's average causal response (ACRT) when no unit is untreated:
  setup then does not check for comparison units, and every group is
  estimated. Estimators of ATT(g,t) need comparison units.

- cband:

  whether or not to report a uniform (instead of pointwise) confidence
  band (default is TRUE)

- alp:

  significance level; default is 0.05

- boot_type:

  which type of bootstrap to use

- bstrap:

  whether to use the multiplier bootstrap (`TRUE`, the default) or
  purely analytical standard errors (`FALSE`) when the group-time
  estimator returns an influence function. `bstrap = FALSE` overrides
  `boot_type` (i.e., `boot_type = "empirical"` is ignored) and only
  supports pointwise confidence intervals – if `cband = TRUE` is also
  requested, `ptetools` falls back to pointwise intervals with a
  warning. Analytical standard errors are not currently supported for
  continuous treatment (`gt_type = "dose"`): with `bstrap = FALSE`, the
  multiplier bootstrap is used instead, with a warning.

- weightsname:

  The name of the column that contains sampling weights. The default is
  NULL, in which case no sampling weights are used.

- gt_type:

  which type of group-time effects are computed. The default is "att".
  Different estimation strategies can implement their own choices for
  `gt_type`

- ret_quantile:

  For functions that compute quantile treatment effects, this is a
  specific quantile at which to report results, e.g.,
  `ret_quantile = 0.5` will return that the qte at the median.

- probs:

  For `gt_type = "qtt"`, a numeric vector of quantile levels at which to
  evaluate the QTT curve (e.g., `seq(0.05, 0.95, 0.05)`). Defaults to
  `seq(0.05, 0.95, 0.05)` when NULL.

- biters:

  number of bootstrap iterations; default is 100. `biters = 0` skips the
  bootstrap: where `attgt_fun` returns an influence function this is the
  same as `bstrap = FALSE` (analytical, pointwise standard errors);
  where standard errors are only available from the bootstrap (the
  empirical bootstrap, dose-specific effects), the point estimates are
  returned with `NA` standard errors.

- cl:

  parallel workers for the empirical bootstrap (the multiplier bootstrap
  does not need them); default is 1 (no parallelization). Either a
  number of workers or a cluster object from
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  A number uses forked processes on Unix/macOS; on Windows, which cannot
  fork, a cluster with that many workers is created and stopped
  automatically. Cluster workers are separate R sessions: they load the
  installed packages, and functions defined in your workspace (e.g. a
  custom `attgt_fun`) must not rely on other objects in your workspace.

- call:

  keeps track of through the `call` from external functions/packages

- ...:

  additional arguments

## Value

`pte_params` object
