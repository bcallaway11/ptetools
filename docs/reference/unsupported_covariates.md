# Covariates dropped for collinearity that matter for prediction

A model fitted on one sample (the units with `D == 0`) and used to
predict for another (`D == 1`) cannot identify coefficients on
covariates that are collinear in the fitting sample, so such covariates
are dropped. That is harmless when, for a predicted unit, the dropped
column equals the same linear combination of the kept columns as in the
fitting sample (duplicate columns, a factor level no unit has).
Otherwise, e.g. a dummy for a region that only treated units belong to,
the prediction implicitly sets that coefficient to zero and extrapolates
for the unit.

This function flags, for each unit with `D == 1` and each dropped
covariate, whether that unit's prediction is affected. It is the check
behind
[`pte_attgt()`](https://github.com/bcallaway11/ptetools/reference/pte_attgt.md)'s
warning about dropped covariates, exported so that other `attgt_fun`s
can apply the same rule, combine flags across fitting samples, weight
them, and report them with
[`pte_cell_warning()`](https://github.com/bcallaway11/ptetools/reference/pte_cell_warning.md).
To fit on treated units and predict for comparison units, pass `1 - D`.

## Usage

``` r
unsupported_covariates(covmat, D, keep_covs = NULL)
```

## Arguments

- covmat:

  numeric covariate matrix for all units, including the intercept;
  column names are used in the result.

- D:

  indicator (0/1 or logical): 0 for the units the model is fitted on, 1
  for the units it predicts for.

- keep_covs:

  indices of the columns of `covmat` kept in the fitted model. The
  default `NULL` uses the rule in
  [`pte_attgt()`](https://github.com/bcallaway11/ptetools/reference/pte_attgt.md):
  a pivoted QR decomposition of the fitting sample's
  `crossprod(covmat) / n`, keeping the first `rank` columns.

## Value

A logical matrix with one row per unit with `D == 1` (in the order of
`covmat`) and one column per dropped covariate that is `TRUE` for at
least one of them. `TRUE` means that unit's value is not the fitting
sample's linear combination of the kept columns. With no such covariates
it has zero columns.

## Examples

``` r
set.seed(1)
D <- rep(0:1, each = 10)
region_c <- c(rep(0, 10), rep(1:0, 5)) # region "c" has treated units only
covmat <- cbind("(Intercept)" = 1, x = rnorm(20), region_c = region_c,
                x_copy = NA)
covmat[, "x_copy"] <- 2 * covmat[, "x"] # redundant for every unit
flags <- unsupported_covariates(covmat, D)
colnames(flags) # "region_c" only
#> [1] "region_c"
colMeans(flags) # share of treated units affected
#> region_c 
#>      0.5 
```
