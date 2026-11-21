# Multiplier Bootstrap

Function for using multiplier bootstrap to conduct inference

## Usage

``` r
mboot2(inffunc, biters = 1000, alp = 0.05, cband = TRUE, cluster = NULL)
```

## Arguments

- inffunc:

  influence function matrix

- biters:

  number of bootstrap iterations; default is 100. `biters = 0` skips the
  bootstrap: where `attgt_fun` returns an influence function this is the
  same as `bstrap = FALSE` (analytical, pointwise standard errors);
  where standard errors are only available from the bootstrap (the
  empirical bootstrap, dose-specific effects), the point estimates are
  returned with `NA` standard errors.

- alp:

  significance level; default is 0.05

- cband:

  whether to compute the critical value for uniform confidence bands
  (default `TRUE`). With `FALSE`, only standard errors are computed and
  `crit_val` is `NA`.

- cluster:

  cluster identifiers, one per row of `inffunc`, or `NULL` (the default)
  for clustering at the level of the rows (units)

## Value

list with the following elements:

- `boot_se`: bootstrap standard errors

- `crit_val`: critical value for uniform confidence bands, from the
  sup-t statistic over the parameters with a positive standard error
  (the normal critical value if there is only one; `NA` if there are
  none or `cband = FALSE`)
