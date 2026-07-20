# ptetools 1.0.1

* Force time periods to be positive integers in `ptetools::setup_pte()`

* Fix bug related to accounting for variance of untreated group with continuous treatment

* Added `covid_data` dataset (previously in `ppe`); updated GitHub Actions workflows; fixed several documentation issues; dropped `tidyr` dependency in favour of `data.table`

* Added `pte_default()`, a convenience wrapper combining `setup_pte()`, `two_by_two_subset()`, and `pte_attgt()` for standard DID/unconfoundedness-style estimation

* Added support for repeated cross-sectional data via `panel = FALSE`, with a new `two_by_two_rcs_subset()` subset function

* Added `print()`, `summary()`, `autoplot()`, and `plot()` methods for `pte_emp_boot` (empirical bootstrap results)

* Added `aggte_fun` argument to `pte()`, `panel_empirical_bootstrap()`, and `qtt_empirical_bootstrap()` for supplying a custom group-time aggregation function on the empirical bootstrap path

* Bumped minimum required `BMisc` version to 1.4.8 — `ptetools` calls several `BMisc` functions (`add_cov_to_formula`, `rhs_vars`, `combine_ecdfs`, `get_list_element`, `weighted_combine_list`) that were not available until that version

# ptetools 1.0.0

* Initial CRAN submission.
