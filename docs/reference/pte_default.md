# Default, General Function for Computing Treatment Effects with Panel Data

This is a generic/example wrapper for a call to the `pte` function.

This function provides access to difference-in-differences and
unconfoundedness based identification/estimation strategies given (i)
panel data and (ii) staggered treatment adoption

## Usage

``` r
pte_default(
  yname,
  gname,
  tname,
  idname = NULL,
  data,
  panel = TRUE,
  xformula = ~1,
  d_outcome = FALSE,
  d_covs_formula = ~-1,
  lagged_outcome_cov = FALSE,
  est_method = "aipw",
  anticipation = 0,
  base_period = "varying",
  control_group = "notyettreated",
  weightsname = NULL,
  clustervars = NULL,
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
  bstrap = TRUE,
  biters = 100,
  cl = 1,
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

- xformula:

  one-sided formula for covariates used in the propensity score and
  outcome regression models

- d_outcome:

  Whether or not to take the first difference of the outcome. The
  default is FALSE. To use difference-in-differences, set this to be
  TRUE.

- d_covs_formula:

  A formula for time varying covariates to enter the first estimation
  step models. The default is not to include any, and, hence, to only
  include pre-treatment covariates.

- lagged_outcome_cov:

  Whether to include the lagged outcome as a covariate. Default is
  FALSE.

- est_method:

  Which type of estimation method to use. The default is "aipw"
  (augmented inverse probability weighting, i.e. doubly robust). The
  other option is "reg" for regression adjustment. "dr" is a deprecated
  alias for "aipw".

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
  [`setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md);
  units first treated after the last period count as eventually
  treated). Different estimation strategies can implement their own
  choices for the control group.

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

- cband:

  whether or not to report a uniform (instead of pointwise) confidence
  band (default is TRUE)

- alp:

  significance level; default is 0.05

- boot_type:

  should be one of "multiplier" (the default) or "empirical". The
  multiplier bootstrap is generally much faster, but `attgt_fun` needs
  to provide an expression for the influence function (which could be
  challenging to figure out). If no influence function is provided, then
  the `pte` package will use the empirical bootstrap no matter what the
  value of this parameter. `bstrap = FALSE` (purely analytical standard
  errors) takes priority over `boot_type`: it is only available when
  `attgt_fun` returns an influence function, and it overrides
  `boot_type = "empirical"`.

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

- ...:

  additional arguments passed to `pte`, such as `min_e`, `max_e`, and
  `balance_e` for controlling the event study range.

## Value

`pte_results` object

## Examples

``` r
# example using minimum wage data
# and a lagged outcome unconfoundedness strategy
library(did)
data(mpdta)
lou_res <- pte_default(
  yname = "lemp",
  gname = "first.treat",
  tname = "year",
  idname = "countyreal",
  data = mpdta,
  xformula = ~lpop,
  d_outcome = FALSE,
  d_covs_formula = ~lpop,
  lagged_outcome_cov = TRUE
)

summary(lou_res)
#> 
#> Overall ATT:  
#>     ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.037        0.0114    -0.0593     -0.0147 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Simult.  Conf. Band]  
#>          -3   0.0262     0.0156       -0.0160      0.0684  
#>          -2  -0.0100     0.0128       -0.0446      0.0245  
#>          -1  -0.0222     0.0144       -0.0611      0.0166  
#>           0  -0.0266     0.0131       -0.0620      0.0088  
#>           1  -0.0448     0.0124       -0.0783     -0.0113 *
#>           2  -0.1312     0.0337       -0.2221     -0.0404 *
#>           3  -0.0968     0.0375       -0.1981      0.0044  
#> ---
#> Signif. codes: `*' confidence band does not cover 0
#> 
ggplot2::autoplot(lou_res)

```
