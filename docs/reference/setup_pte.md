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
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
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

- cband:

  whether or not to report a uniform (instead of pointwise) confidence
  band (default is TRUE)

- alp:

  significance level; default is 0.05

- boot_type:

  which type of bootstrap to use

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

  number of bootstrap iterations; default is 100

- cl:

  number of clusters to be used when bootstrapping; default is 1

- call:

  keeps track of through the `call` from external functions/packages

- ...:

  additional arguments

## Value

`pte_params` object
