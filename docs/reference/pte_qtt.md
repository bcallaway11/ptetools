# Class for QTT Curve Results

Holds the full quantile treatment effect (QTT) curve at the overall,
group-specific, and dynamic (event-study) aggregation levels. Each
aggregation contains estimates at all quantile levels in `probs`
together with bootstrap standard errors and pointwise confidence
intervals.

## Usage

``` r
pte_qtt(
  overall,
  dynamic,
  group,
  F0_overall = NULL,
  F1_overall = NULL,
  ptep,
  att = NULL,
  qtt_minus_att = NULL
)
```

## Arguments

- overall:

  data.frame with columns `probs`, `qtt`, `se`, `lower_pw`, `upper_pw`,
  `lower_ub`, `upper_ub`

- dynamic:

  data.frame with columns `e`, `probs`, `qtt`, `se`, `lower_pw`,
  `upper_pw`, `lower_ub`, `upper_ub`

- group:

  data.frame with columns `group`, `probs`, `qtt`, `se`, `lower_pw`,
  `upper_pw`, `lower_ub`, `upper_ub`

- F0_overall:

  mixture CDF of untreated potential outcomes

- F1_overall:

  mixture CDF of treated potential outcomes

- ptep:

  `pte_params` object

- att:

  `NULL`, or a list with the `overall`, `dynamic` and `group` ATTs from
  the same run and bootstrap draws: data.frames with columns (`e` or
  `group`,) `att`, `se`, `lower`, `upper` (pointwise)

- qtt_minus_att:

  `NULL`, or a list with the `overall`, `dynamic` and `group` curves
  QTT(tau) - ATT, with the same columns as the QTT results (value column
  `qtt_minus_att`) plus `pval`: the bootstrap sup-t p-value of the test
  that QTT(tau) = ATT for every tau in `probs`, one per curve (repeated
  on its rows)

## Value

a `pte_qtt` object
