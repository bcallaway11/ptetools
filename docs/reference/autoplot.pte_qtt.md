# autoplot.pte_qtt

Plot a `pte_qtt` object.

For `type = "overall"`: QTT curve with quantile on the x-axis.

For `type = "dynamic"`: event-study plot with event time on the x-axis.
Each selected quantile is a separate colored line. CIs are shown by
default when a single quantile is plotted, and suppressed by default
when multiple quantiles are plotted.

## Usage

``` r
# S3 method for class 'pte_qtt'
autoplot(
  object,
  type = "overall",
  cband = TRUE,
  plot_probs = 0.5,
  plot_ci = NULL,
  ...
)
```

## Arguments

- object:

  a `pte_qtt` object

- type:

  which aggregation to plot: `"overall"` (default) or `"dynamic"`.
  `"group"` is a stub.

- cband:

  logical; if `TRUE` (default), show uniform confidence band; if
  `FALSE`, show pointwise intervals. Applies when CIs are displayed.

- plot_probs:

  numeric vector of quantile levels to show in the dynamic plot.
  Defaults to `0.5` (median). All values must be present in
  `object$dynamic$probs`.

- plot_ci:

  logical or `NULL`. If `NULL` (default), CIs are shown when
  `length(plot_probs) == 1` and suppressed otherwise. Set `TRUE` to
  always show CIs, `FALSE` to never show them.

- ...:

  unused

## Value

a `ggplot` object
