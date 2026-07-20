# ptetools 1.0.1

* Force time periods to be positive integers in `ptetools::setup_pte()`

* Fix bug related to accounting for variance of untreated group with continuous treatment

* Added `covid_data` dataset (previously in `ppe`); updated GitHub Actions workflows; fixed several documentation issues; dropped `tidyr` dependency in favour of `data.table`

* Added `aggte_fun` argument to `pte()`, `panel_empirical_bootstrap()`, and `qtt_empirical_bootstrap()` for supplying a custom group-time aggregation function on the empirical bootstrap path

# ptetools 1.0.0

* Initial CRAN submission.
