# ptetools 1.0.2

* `cl` now parallelizes the empirical bootstrap on Windows too (#11). A
  numeric `cl` only worked by forking, which Windows does not support, so
  there the bootstrap silently ran serially. On Windows, ptetools now
  creates a cluster with `cl` workers (reproducible with `set.seed()`) and
  stops it afterwards. `cl` can also be a cluster object from
  `parallel::makeCluster()`, on any platform. The documentation of `cl` now
  says what it does.

* The pre-test of parallel trends is now reported with
  `base_period = "universal"` (#18). The reference cells, which are 0 by
  construction, made the covariance matrix singular. Cells with zero
  variance are now left out, as in `did::att_gt()`, and the pre-test
  matches `did` for both base periods.

* Fixed standard errors of aggregated parameters (overall ATT, event study,
  group effects) from `pte_aggte()`, which were slightly off (about 0.3-0.8%
  on `mpdta`) (#19). The term for estimating the group shares used shares
  normalized to sum to 1 across treated groups instead of P(G = g). They now
  match `did::aggte()` exactly. Point estimates are unchanged.

* `overall_weights()` (used for continuous treatments) no longer stops with
  "something's going wrong calculating overall weights" when the weights
  sum to 1 only up to floating-point rounding, which happened with many
  timing groups (#1).

* The `bstrap` documentation no longer refers to `dev/NOTES.md`, which is
  not part of the package; it now says what happens with a continuous
  treatment (#4).

* `anticipation` now follows `did::att_gt()`: it is measured in units of the
  time variable (e.g., years), not in number of periods, which matters when
  periods are not one unit apart. Specifically:
  * the base period is the last period at least `anticipation` before
    treatment, and cells with `t >= g - anticipation` are post-treatment;
  * not-yet-treated comparison units must not be anticipating treatment in
    either period of the cell (first treated more than `anticipation` after
    both periods), so units about to anticipate are no longer used as
    controls;
  * the pre-test leaves out anticipation periods;
  * units first treated more than `anticipation` after the last period are
    treated as never treated (previously an error);
  * a clear error is given when no group has a period early enough to serve
    as its base period.

  Results with `anticipation = 0` are unchanged. With `anticipation > 0`,
  results can change even with consecutive periods, because of the
  comparison-group rule.

* New exported helpers for custom subset functions: `pte_base_period()`
  gives the base period of a group-time cell as in `did::att_gt()`, and
  `pte_calendar_time()` maps internal period indices back to the time
  variable. `two_by_two_subset()` uses them.

* Fixed pre-treatment `ATT(g,t)` with `base_period = "universal"` and
  not-yet-treated controls (the default) (#16). The comparison group could
  include units already treated in the base period, which comes after the
  current period in pre-treatment cells. Not-yet-treated controls are now
  units first treated after both periods of the cell, as in
  `did::att_gt()`. Post-treatment estimates and the overall ATT are
  unchanged; pre-trends in event studies can change. `pte_default()` now
  matches `did::att_gt()` for both base periods and both control groups,
  including with unequally spaced time periods.

* `biters = 0` now skips the bootstrap (#14). It crashed the multiplier
  bootstrap, and earlier versions silently ran 2 bootstrap draws. Where
  `attgt_fun` returns an influence function, `biters = 0` is the same as
  `bstrap = FALSE` (analytical, pointwise standard errors). Where standard
  errors are only available from the bootstrap (the empirical bootstrap,
  including the QTT estimators, and `ATT(d)`/`ACRT(d)` with a continuous
  treatment), the point estimates are returned with `NA` standard errors.
  `biters` must now be a non-negative whole number.

* Fixed estimates when a column named in `gname`, `tname`, `idname` or
  `yname` is called `G`, `period`, `id` or `Y` (#13). With `gname = "G"` or
  `tname = "period"` and groups coded as calendar years, the overall ATT and
  event study were silently `NaN`. `setup_pte()` and `setup_pte_basic()` now
  store their internal columns as `.G`, `.id`, `.period` and `.Y`, and never
  modify the user's columns.

* **Breaking change for custom setup and subset functions.** The full data
  from a setup function now has the internal columns `.G`, `.id`, `.period`,
  `.Y`, `.w` and `.rowid`, not `G`, `id`, `period` and `Y`. Subset functions
  should select rows using the dot columns and finish with
  `gt_data_frame()`, which gives each group-time cell the columns `G`, `id`,
  `period` and `Y` (`attgt_fun`s are unchanged). `gt_data_frame()` now
  errors if the dot columns are missing. `keep_all_untreated_subset()` and
  `keep_all_pretreatment_subset()` now also return `gt_data_frame` cells.

* Fixed the standard errors of `ATT(d)` and `ACRT(d)` from
  `process_dose_gt()` (used by `contdid`) when `dvals` does not span
  exactly the range of the positive doses: the influence functions were
  built with a different B-spline basis than the one the dose regression
  was fitted with. `process_dose_gt()` now uses the boundary knots in
  `ptep$boundary_knots` when present, and otherwise keeps the previous
  behavior (#7; thanks to @AsadGudzevic94).

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
