# Class for QTT Curve Results

Holds the full quantile treatment effect (QTT) curve at the overall,
group-specific, and dynamic (event-study) aggregation levels. Each
aggregation contains estimates at all quantile levels in `probs`
together with bootstrap standard errors and pointwise confidence
intervals.

## Usage

``` r
pte_qtt(overall, dynamic, group, F0_overall = NULL, F1_overall = NULL, ptep)
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

## Value

a `pte_qtt` object
