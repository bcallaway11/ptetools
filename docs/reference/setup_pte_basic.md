# Basic Setup Function

This is a lightweight (example) function for how to setup the data to be
used in the `ptetools` package.

`setup_pte_basic` takes in information about the structure of `data` and
returns a `pte_params` object. The key piece of information that is
computed by this function is the list of groups and list of time periods
where ATT(g,t) should be computed. In particular, this function omits
the never-treated group but includes all other groups and drops the
first time period. This setup is basically geared towards the 2x2 case —
i.e., where ATT could be identified with two periods, a treated and
untreated group, and the first period being pre-treatment for both
groups. This is the relevant case for DID, but is also relevant for
other cases as well. However, for example, if more pre-treatment periods
were needed, then this function should be replaced by something else.

For code that is written with the idea of being easy-to-use by other
researchers, this is a good place to do some error handling / checking
that the data is in the correct format, etc.

## Usage

``` r
setup_pte_basic(
  yname,
  gname,
  tname,
  idname = NULL,
  data,
  panel = TRUE,
  weightsname = NULL,
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
  bstrap = TRUE,
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

- weightsname:

  The name of the column that contains sampling weights. The default is
  NULL, in which case no sampling weights are used.

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
