# autoplot.pte_qtt

Plot a `pte_qtt` object.

For `type = "overall"`: QTT curve with quantile on the x-axis. When the
object holds the overall ATT from the same run, it is drawn as a
horizontal line, with its pointwise confidence interval shaded
(`show_att`).

For `type = "qtt_minus_att"`: the overall curve QTT(tau) - ATT, with its
band from the same bootstrap draws. A band that excludes 0 at some
quantile rejects QTT(tau) = ATT for all tau.

For `type = "dynamic"`: event-study plot with event time on the x-axis.
Each selected quantile is a separate colored line. CIs are shown by
default when a single quantile is plotted, and suppressed by default
when multiple quantiles are plotted.

For `type = "group"`: each cohort's QTT at the selected quantile(s),
with cohorts on the x-axis; with several quantiles, one colored point
per quantile. CIs as for `type = "dynamic"`.

## Usage

``` r
# S3 method for class 'pte_qtt'
autoplot(
  object,
  type = "overall",
  cband = TRUE,
  plot_probs = 0.5,
  plot_ci = NULL,
  show_att = TRUE,
  ...
)
```

## Arguments

- object:

  a `pte_qtt` object

- type:

  which aggregation to plot: `"overall"` (default), `"dynamic"`,
  `"group"` or `"qtt_minus_att"`.

- cband:

  logical; if `TRUE` (default), show uniform confidence band; if
  `FALSE`, show pointwise intervals. Applies when CIs are displayed.

- plot_probs:

  numeric vector of quantile levels to show in the dynamic and group
  plots. Defaults to `0.5` (median). All values must be among the
  estimated quantiles.

- plot_ci:

  logical or `NULL`. If `NULL` (default), CIs are shown when
  `length(plot_probs) == 1` and suppressed otherwise. Set `TRUE` to
  always show CIs, `FALSE` to never show them.

- show_att:

  logical; for `type = "overall"`, whether to draw the overall ATT
  (default `TRUE`, when available).

- ...:

  unused

## Value

a `ggplot` object
