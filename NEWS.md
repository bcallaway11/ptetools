# ptetools 1.0.2

* Fixed `pte_attgt()`, `did_attgt()` and `covid_attgt()` (and so
  `pte_default()`) giving wrong estimates with covariates or sampling weights
  when the data were not sorted by id within each period: covariates and
  weights were paired with other units' outcomes (#9). `setup_pte()` now sorts
  panel data by id and period, and the 2x2 cells are matched by id. Results
  for data already sorted this way are unchanged.

* `pte()` and `pte_default()` are faster, with the same results; the empirical
  bootstrap (used by the `qte` estimators) is often more than 10x faster.
  Speedups:
  * The bootstrap resamples units by index.
  * Each (g,t) cell is built from row indices computed once.
  * `pte_default()` keeps only the columns it uses.
  * The multiplier bootstrap is vectorized and uses the same random draws as
    before.
  * `data.table` is no longer a dependency.

* The multiplier-bootstrap standard errors for ATT(g,t) now use `biters`
  draws. Previously they always used 1000 draws, whatever `biters` was set to.
  Aggregated parameters already used `biters`.

* `setup_pte()` now gives a clear error when a value of `gname` is neither 0
  nor one of the time periods.

* Fixed standard errors for aggregated parameters (overall ATT, event study)
  when `weightsname` is used: they depended on the scale of the weights and
  were far too large for weights not near 1, such as population counts (#8).
  Weights are now normalized to mean 1 before aggregation, as in `did`.
  Point estimates are unchanged.

* Added `bstrap` argument to `pte()`, `pte_default()`, `setup_pte()`, and
  `setup_pte_basic()` to request purely analytical (non-bootstrap) standard
  errors (`bstrap = FALSE`) instead of the multiplier bootstrap, wherever
  `attgt_fun` returns an influence function. This skips the (often
  expensive) multiplier bootstrap entirely and is much faster for large
  panels. `bstrap = FALSE` only supports pointwise confidence intervals
  (falls back from `cband = TRUE` with a warning), overrides
  `boot_type = "empirical"`, and is not currently supported for continuous
  treatment (`gt_type = "dose"`).

* Fixed a bug in `crit_val_checks()` that threw `object 'dp' not found` when
  a simultaneous critical value could not be computed (e.g. designs with no
  pre-treatment periods, where standard errors come back `NA`). The fallback
  to pointwise critical values now works correctly, and `pte()`/`process_dose_gt()`
  correctly downgrade their `cband` flag so `summary()` reports "Pointwise"
  rather than "Simult." whenever this fallback occurs.

* Fixed a bug where `autoplot.dose_obj()` (used by `contdid::ggcont_did()`)
  crashed with `arguments imply differing number of rows` because the
  critical value stored on `dose_obj` carried a stray `"cband"` attribute
  left over from `crit_val_checks()`, which broke `data.frame()` recycling.

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
