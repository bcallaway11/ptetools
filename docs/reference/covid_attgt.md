# Covid ATT(g,t) Estimator

Computes a group-time average treatment effect and influence function
using an unconfoundedness-type identification strategy. This estimator
is appropriate when parallel trends is implausible but a
selection-on-observables assumption holds in levels (rather than
differences) — e.g., during the early COVID-19 pandemic.

Originally from Callaway and Li (2021). Moved into `ptetools` from the
`ppe` package.

## Usage

``` r
covid_attgt(gt_data, xformla, d_outcome = FALSE, d_covs_formula = ~-1, ...)
```

## Arguments

- gt_data:

  data that is "local" to a particular group-time average treatment
  effect, structured as a `gt_data_frame`

- xformla:

  one-sided formula for covariates used in the propensity score and
  outcome regression models

- d_outcome:

  logical; if `TRUE`, use first-differenced outcomes. Default is `FALSE`
  (levels).

- d_covs_formula:

  one-sided formula for covariates to include as changes (differences).
  Default is `~-1` (no change covariates).

- ...:

  extra arguments; not used

## Value

`attgt_if` object

## References

Callaway, B. and Li, T. (2021). Policy Evaluation during a Pandemic.
<https://arxiv.org/abs/2105.06927>
