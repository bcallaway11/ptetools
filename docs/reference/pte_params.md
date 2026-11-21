# PTE Parameters Class

Class that contains pte parameters

## Usage

``` r
pte_params(
  yname,
  gname,
  tname,
  idname = NULL,
  data,
  panel = TRUE,
  glist,
  tlist,
  cband,
  alp,
  boot_type,
  bstrap = TRUE,
  anticipation = NULL,
  base_period = NULL,
  weightsname = NULL,
  clustervars = NULL,
  control_group = "notyettreated",
  gt_type = "att",
  ret_quantile = 0.5,
  probs = NULL,
  global_fun = FALSE,
  time_period_fun = FALSE,
  group_fun = FALSE,
  biters,
  cl,
  call = NULL
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

- glist:

  list of groups to create group-time average treatment effects for

- tlist:

  list of time periods to create group-time average treatment effects
  for

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

- weightsname:

  The name of the column that contains sampling weights. The default is
  NULL, in which case no sampling weights are used.

- clustervars:

  The name of a variable to cluster standard errors on, in addition to
  `idname` (as in
  [`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html)):
  one variable, constant over time within each unit (e.g. state when
  units are counties). The default is NULL, which clusters at the unit
  level. Clustering applies to analytical standard errors, the
  multiplier bootstrap (one multiplier per cluster) and the empirical
  bootstrap (whole clusters are resampled).

- control_group:

  Which group is used as the comparison group. The default choice is
  "notyettreated" (units not yet treated in either period of a
  group-time comparison, including never-treated units). "nevertreated"
  uses only never-treated units, and "eventuallytreated" uses
  not-yet-treated units excluding the never treated (as in
  [`setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md);
  units first treated after the last period count as eventually
  treated). Different estimation strategies can implement their own
  choices for the control group.

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

- global_fun:

  Logical indicating whether or not untreated potential outcomes can be
  estimated in one shot, i.e., for all groups and time periods. Main use
  case would be for one-shot imputation estimators. Not supported yet.

- time_period_fun:

  Logical indicating whether or not untreated potential outcomes can be
  estimated for all groups in the same time period. Not supported yet.

- group_fun:

  Logical indicating whether or not untreated potential outcomes can be
  estimated for all time periods for a single group. Not supported yet.
  These functions aim at reducing or eliminating running the same code
  multiple times.

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

## Value

`pte_params` object
