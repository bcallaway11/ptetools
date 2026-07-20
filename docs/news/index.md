# Changelog

## ptetools 1.0.1

- Force time periods to be positive integers in
  [`ptetools::setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md)

- Fix bug related to accounting for variance of untreated group with
  continuous treatment

- Added `covid_data` dataset (previously in `ppe`); updated GitHub
  Actions workflows; fixed several documentation issues; dropped `tidyr`
  dependency in favour of `data.table`

- Added
  [`pte_default()`](https://github.com/bcallaway11/ptetools/reference/pte_default.md),
  a convenience wrapper combining
  [`setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md),
  [`two_by_two_subset()`](https://github.com/bcallaway11/ptetools/reference/two_by_two_subset.md),
  and
  [`pte_attgt()`](https://github.com/bcallaway11/ptetools/reference/pte_attgt.md)
  for standard DID/unconfoundedness-style estimation

- Added support for repeated cross-sectional data via `panel = FALSE`,
  with a new
  [`two_by_two_rcs_subset()`](https://github.com/bcallaway11/ptetools/reference/two_by_two_rcs_subset.md)
  subset function

- Added [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), `autoplot()`, and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  `pte_emp_boot` (empirical bootstrap results)

- Added `aggte_fun` argument to
  [`pte()`](https://github.com/bcallaway11/ptetools/reference/pte.md),
  [`panel_empirical_bootstrap()`](https://github.com/bcallaway11/ptetools/reference/panel_empirical_bootstrap.md),
  and
  [`qtt_empirical_bootstrap()`](https://github.com/bcallaway11/ptetools/reference/qtt_empirical_bootstrap.md)
  for supplying a custom group-time aggregation function on the
  empirical bootstrap path

## ptetools 1.0.0

CRAN release: 2025-02-13

- Initial CRAN submission.
