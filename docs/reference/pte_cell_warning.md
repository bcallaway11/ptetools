# Warn about a problem in one group-time cell

For use inside an `attgt_fun`. Signals a warning that
[`compute.pte()`](https://github.com/bcallaway11/ptetools/reference/compute.pte.md)
(and so
[`pte()`](https://github.com/bcallaway11/ptetools/reference/pte.md))
collects across group-time cells instead of issuing one warning per
cell. After the loop over cells, it gives one warning per distinct
`summary`, listing each affected cell (with the group and period in the
original coding of `gname` and `tname`) and its `detail`. Warnings are
reported for the original sample only, not again for each empirical
bootstrap draw.

Called outside
[`compute.pte()`](https://github.com/bcallaway11/ptetools/reference/compute.pte.md),
it is an ordinary warning with message `"<summary>: <detail>"`. The
condition has class `"pte_cell_warning"`.

## Usage

``` r
pte_cell_warning(summary, detail)
```

## Arguments

- summary:

  one line describing the kind of problem. Cells with the same `summary`
  are reported together, so it should not depend on the cell.

- detail:

  what happened in this cell, e.g. a share of treated units or the names
  of covariates.

## Value

Called for its side effect (a warning); returns `NULL` invisibly when
the warning is muffled.

## Examples

``` r
my_attgt <- function(gt_data, ...) {
  n_treated <- sum(gt_data$D[gt_data$name == "post"])
  if (n_treated < 50) {
    pte_cell_warning("Few treated units", paste(n_treated, "treated units"))
  }
  pte_attgt(gt_data, xformula = ~1, d_outcome = TRUE)
}
if (requireNamespace("did", quietly = TRUE)) {
  # one warning, listing the cells of the 2004 and 2006 cohorts
  res <- pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = did::mpdta, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = my_attgt, biters = 0
  )
}
#> Warning: Few treated units (8 cells):
#>   group 2004, period 2004: 20 treated units
#>   group 2006, period 2004: 40 treated units
#>   group 2004, period 2005: 20 treated units
#>   group 2006, period 2005: 40 treated units
#>   group 2004, period 2006: 20 treated units
#>   group 2006, period 2006: 40 treated units
#>   group 2004, period 2007: 20 treated units
#>   group 2006, period 2007: 40 treated units
```
