# plot.pte_qtt

Convenience wrapper around
[`autoplot.pte_qtt`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_qtt.md).

## Usage

``` r
# S3 method for class 'pte_qtt'
plot(x, type = "overall", cband = TRUE, plot_probs = 0.5, plot_ci = NULL, ...)
```

## Arguments

- x:

  a `pte_qtt` object

- type:

  which aggregation to plot. See
  [`autoplot.pte_qtt`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_qtt.md).

- cband:

  logical; if `TRUE` (default), show uniform confidence band.

- plot_probs:

  numeric vector of quantile levels to show. See
  [`autoplot.pte_qtt`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_qtt.md).

- plot_ci:

  logical or `NULL`. See
  [`autoplot.pte_qtt`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_qtt.md).

- ...:

  passed to `autoplot.pte_qtt`

## Value

invisibly returns the `ggplot` object
