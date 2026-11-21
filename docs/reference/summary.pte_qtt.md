# summary.pte_qtt

Summary for a `pte_qtt` object: the overall QTT curve, and the
event-study and group QTTs at selected quantiles. When the object holds
the ATTs from the same run
([`qtt_pte_aggregations()`](https://github.com/bcallaway11/ptetools/reference/qtt_pte_aggregations.md)),
also the overall and event-study ATTs, with the p-value of the test that
QTT(tau) = ATT at every estimated quantile. Confidence bands are uniform
over quantiles (for each curve) when `cband = TRUE` was used for
estimation, and pointwise otherwise.

## Usage

``` r
# S3 method for class 'pte_qtt'
summary(object, probs = 0.5, ...)
```

## Arguments

- object:

  a `pte_qtt` object

- probs:

  quantile levels at which to report the event-study and group QTTs
  (default `0.5`, the median). Must be among the estimated quantiles.

- ...:

  additional arguments

## Value

a `summary.pte_qtt` object, printed by its `print` method
