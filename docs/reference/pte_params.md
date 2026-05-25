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
  anticipation = NULL,
  base_period = NULL,
  weightsname = NULL,
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

- anticipation:

  how many periods before the treatment actually takes place that it can
  have an effect on outcomes

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

- control_group:

  Which group is used as the comparison group. The default choice is
  "notyettreated", but different estimation strategies can implement
  their own choices for the control group

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

  number of bootstrap iterations; default is 100

- cl:

  number of clusters to be used when bootstrapping; default is 1

- call:

  keeps track of through the `call` from external functions/packages

## Value

`pte_params` object
