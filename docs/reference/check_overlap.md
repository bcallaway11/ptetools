# Overlap Diagnostics for Group-Time Cells

Covariate-adjusted estimates of \\ATT(g,t)\\ need overlap in every
group-time cell: each treated unit needs comparison units with similar
covariates. `check_overlap()` reports, for each cell, how much of the
treated group lacks that support.

The cells are exactly the ones
[`pte_default()`](https://github.com/bcallaway11/ptetools/reference/pte_default.md)
estimates with (same comparison group, base period and anticipation),
and the covariates are built the same way as in
[`pte_attgt()`](https://github.com/bcallaway11/ptetools/reference/pte_attgt.md):
the variables in `xformula` at the base period, plus changes in the
variables in `d_covs_formula` and, with `lagged_outcome_cov = TRUE`, the
base-period outcome.

Two measures are reported for each cell:

- **Range support**: the share of treated units with at least one
  covariate outside the comparison group's range (its minimum and
  maximum). For a binary covariate, this is a value no comparison unit
  has. Being inside the range is necessary, not sufficient, for overlap,
  but the check works in every cell.

- **Propensity score**: the share of treated units whose estimated
  propensity score (a logit, as in
  [`pte_attgt()`](https://github.com/bcallaway11/ptetools/reference/pte_attgt.md))
  is above each of `pscore_thresholds`. Reported only when the cell has
  more treated units than covariates (`pscore_ok`); with fewer, the
  logit separates whatever the overlap.

With `weightsname`, every share is also reported weighted by the
sampling weights (columns ending in `_w`). A few heavy treated units
outside the comparison range can carry much of a weighted ATT.

This is a diagnostic only; it does not trim or change any estimate.

## Usage

``` r
check_overlap(
  yname,
  gname,
  tname,
  idname,
  data,
  xformula = ~1,
  d_covs_formula = ~-1,
  lagged_outcome_cov = FALSE,
  weightsname = NULL,
  control_group = "notyettreated",
  base_period = "varying",
  anticipation = 0,
  panel = TRUE,
  pscore_thresholds = c(0.9, 0.999)
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

- xformula:

  one-sided formula for covariates used in the propensity score and
  outcome regression models

- d_covs_formula:

  A formula for time varying covariates to enter the first estimation
  step models. The default is not to include any, and, hence, to only
  include pre-treatment covariates.

- lagged_outcome_cov:

  Whether to include the lagged outcome as a covariate. Default is
  FALSE.

- weightsname:

  The name of the column that contains sampling weights. The default is
  NULL, in which case no sampling weights are used.

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

- anticipation:

  how long before the treatment actually takes place it can already
  affect outcomes, in units of the time variable `tname` (e.g., years),
  as in
  [`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html).
  With unequally spaced periods this is not the same as a number of
  periods. Units first treated more than `anticipation` after the last
  period are treated as never treated.

- panel:

  Whether the data are panel data. The default is TRUE. Set to FALSE for
  repeated cross sections.

- pscore_thresholds:

  Propensity score thresholds. The default `c(0.9, 0.999)` covers the
  usual trimming rule of thumb (Crump et al., 2009) and the default of
  [`did::trimmer()`](https://bcallaway11.github.io/did/reference/trimmer.html).

## Value

A data frame of class `pte_overlap`, one row per group-time cell, with
columns

- group, time.period:

  the cell, in units of `tname`

- post:

  whether the cell is post-treatment

- n_treated, n_comparison:

  numbers of treated and comparison units

- n_covariates:

  number of linearly independent covariates, including the intercept

- pscore_ok:

  whether the propensity score can be estimated (more treated units than
  covariates)

- outside_range:

  share of treated units outside the comparison range on at least one
  covariate

- worst_covariate:

  the covariate with the most treated units outside the comparison range
  (`NA` if none)

- pscore_above\_:

  share of treated units with a propensity score above each threshold
  (`NA` if `pscore_ok` is `FALSE`)

plus weighted versions of the shares (ending in `_w`) when `weightsname`
is given. With `xformula = ~1` and no other covariates there is nothing
to check, and the shares are `NA`.

## References

Crump, R. K., Hotz, V. J., Imbens, G. W. and Mitnik, O. A. (2009).
Dealing with limited overlap in estimation of average treatment effects.
*Biometrika*, 96(1), 187-199.

## Examples

``` r
ov <- check_overlap(
  yname = "lemp", gname = "first.treat", tname = "year",
  idname = "countyreal", data = did::mpdta, xformula = ~lpop
)
ov
#> Overlap diagnostics for 12 group-time cells (7 post-treatment)
#> Post-treatment cells with treated units outside the comparison range: 1 
#> 
#> Worst cells:
#>  group time.period n_treated n_comparison outside_range worst_covariate
#>   2007        2007       131          309       0.00763            lpop
#>  pscore_above_0.9 pscore_above_0.999
#>                 0                  0
```
