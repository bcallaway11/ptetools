# General ATT(g,t)

`pte_attgt` takes a "local" data.frame and computes an estimate of a
group time average treatment effect and a corresponding influence
function. This function generalizes a number of existing methods and
underlies the `pte_default` function.

The code relies on `gt_data` having certain variables defined. In
particular, there should be an `id` column (individual identifier), `G`
(group identifier), `period` (time period), `name` (equal to "pre" for
pre-treatment periods and equal to "post" for post treatment periods),
`Y` (outcome).

In our case, we call `two_by_two_subset` which sets up the data to have
this format before the call to `pte_attgt`

## Usage

``` r
pte_attgt(
  gt_data,
  xformula,
  d_outcome = FALSE,
  d_covs_formula = ~-1,
  lagged_outcome_cov = FALSE,
  est_method = "dr",
  ...
)
```

## Arguments

- gt_data:

  data that is "local" to a particular group-time average treatment
  effect

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

  Which type of estimation method to use. Default is "dr" for doubly
  robust. The other option is "reg" for regression adjustment.

- ...:

  extra function arguments; not used here

## Value

attgt_if
