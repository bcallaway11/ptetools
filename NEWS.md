# ptetools 1.1.0

* Bug fix: with time periods coded 2, ..., T (for example 2:3 or 2:5),
  results were not mapped back from the internal coding 1, ..., T.
  Group-time results were labeled with the wrong groups and periods, and
  the overall ATT and event study were `NaN`. Whether to map back was
  guessed from whether the internal periods were all valid original
  periods; it now checks whether the setup function recoded them. Periods
  coded 1, ..., T, calendar years and other codings were unaffected.
* Bug fix: the empirical bootstrap ignored sampling weights. Each draw was
  set up again without `weightsname`, so every draw was unweighted and
  weighted standard errors and bands were those of the unweighted
  estimator (#21). This affected every estimator using
  `boot_type = "empirical"` with weights, including QTT runs and qte's
  estimators. Point estimates were correct. The QOTT bootstrap also
  rebuilt its draws without `ret_quantile`, so its standard errors were
  for the median whatever quantile was estimated.

* Sampling weights audit (#21): `did_attgt()` and `covid_attgt()` now use
  sampling weights (they did not pass them to DRDID), and
  `setup_pte_basic()` gains `weightsname` (it set every weight to 1).
  Weighted estimates and standard errors from `pte_default()` match
  `did::att_gt()` for panel data (ATT(g,t), event study and overall, both
  estimation methods, with covariates, both base periods and comparison
  groups, and with clustering) and for repeated cross sections (ATT(g,t)),
  and are unchanged when the weights are rescaled.

* Bug fix: aggregated QTTs and QOTTs mixed the wrong cells' distributions.
  `attgt_pte_aggregations()` returned its weight vectors in the order of an
  internal table that `merge()` sorts by group, while the cells' `F0`/`F1`
  are in the order the cells were computed. With more than one group, the
  overall, event-time and group QTTs could put weight on pre-treatment
  cells and none on some post-treatment cells. Cells dropped as `NA` also
  shifted later weights. The weights now follow the order of the cells.
  ATT results were not affected. Found while adding the ATT to QTT runs
  (#27).

* QTT runs (`gt_type = "qtt"` with `qtt_pte_aggregations()`) now also
  return the ATT, from the same cells, weights and bootstrap draws (#27):
  `$att` holds the overall, event-time and group ATTs with standard errors
  and confidence intervals, and `$qtt_minus_att` holds the curves
  QTT(tau) - ATT with pointwise and uniform bands and the p-value of a
  sup-t test that QTT(tau) = ATT at every quantile (no distributional
  effect beyond a location shift). Overlaying a separately estimated ATT
  on a QTT band is not a valid test of this, since it ignores the
  estimation error in the ATT and its correlation with the QTT.
  `summary()` reports the ATTs and the tests, `autoplot()` draws the
  overall ATT on the QTT curve, and `autoplot(type = "qtt_minus_att")`
  plots the difference with its band.

* Aggregated QTTs (`qtt_pte_aggregations()`) are now exact (#25). The
  cells' distribution functions used to be mixed on a 1,000-point grid of
  the pooled outcome, so each quantile was rounded up to a grid point, and
  `quantile()` on the mixture then rounded its jumps again. Weighted data
  and the equivalent row-replicated data gave different QTTs. The mixture
  now uses every support point of the cells' CDFs (one sort), at about the
  same cost, through `BMisc::combine_ecdfs()`, which computes mixtures
  exactly in BMisc 1.4.11 (now required). Reported QTTs change slightly (on `mpdta`, by up to about
  0.01). The cells' `F0`/`F1` must be step-function CDFs, as those from
  `BMisc::weighted_ecdf()` are. Bootstrap draws no longer keep their own
  overall CDFs, so memory does not grow with sample size times `biters`.
  The quantile of the treatment effect (`qott_pte_aggregations()`) is now
  exact in the same way. Its grid, 1,000 points on [-max(y), max(y)], also
  missed effects below -max(y), and ran backwards when every outcome was
  negative. Each cell's QOTT is now its exact quantile too.

* The doubly robust estimator is now `est_method = "aipw"` (augmented
  inverse probability weighting), the new default in `pte_default()`,
  `pte_attgt()` and `did_rcs_attgt()` (#30). `"dr"` still works as a
  deprecated alias, with a warning; it was ambiguous next to qte's
  `cond_dist = "dr"` (distribution regression). An unsupported
  `est_method` now gives a clear error up front.

* New `control_group = "eventuallytreated"` (#29): the comparison units are
  units that are treated at some point but not yet treated (net of
  anticipation) in either period of a group-time comparison, i.e. the
  not-yet-treated units other than the never treated. The latest-treated
  cohort then serves only as a comparison group, and periods from its
  treatment date on are dropped, as without a never-treated group. Units
  first treated after the last period count as eventually treated.

* QTT results (`pte_qtt`), e.g. from qte's estimators (#26):
  * `summary()` now returns a `summary.pte_qtt` object instead of printing,
    so the table no longer appears twice at the console;
  * the summary shows the dynamic (event-study) and group QTTs at selected
    quantiles (`probs`, default the median), not only the overall curve;
  * it labels the band (simultaneous over quantiles, or pointwise) and marks
    estimates whose band excludes 0 with `*`, like the ATT summary;
  * `autoplot(type = "group")` now plots each cohort's QTT at the selected
    quantile(s); it used to error.

* `setup_pte()` gains `require_comparison` (default `TRUE`). With `FALSE`,
  setup does not require comparison units and estimates every group; this
  is for estimators that use only variation within each group and build
  their own cells, such as contdid's ACRT when no unit is untreated
  (contdid #5). `process_dose_gt()` then reports only ACRT results; ATT(d)
  and the overall ATT are `NA`.

* `control_group` is now checked: it must be `"notyettreated"`,
  `"nevertreated"` or `"eventuallytreated"`. Any other value, including a
  typo, used to be treated as `"nevertreated"` without a warning.

* Fixed designs without a never-treated group. `setup_pte()` (and
  `setup_pte_basic()`) removed the smallest group value assuming it was 0,
  so without never-treated units the earliest cohort was silently dropped
  and estimation could then fail. Now, as in `did::att_gt()`, the latest
  cohort serves as the comparison group (as never treated with
  `control_group = "nevertreated"`, with a warning) and periods from its
  treatment date on are dropped. If every unit is treated in the same
  period, there is now a clear error that no comparison units exist.

* Fixed the empirical-bootstrap aggregation with `weightsname`: cohorts
  were combined by unweighted unit counts instead of weighted shares, so
  overall and event-study results were wrong whenever average weights
  differed across cohorts (#23). This affected estimators that use the
  empirical bootstrap, including qte's. They now match `pte_aggte()`.

* Sampling weights are now checked (#24): they must be non-missing, finite
  and non-negative with a positive sum, and, with panel data, constant over
  time within each unit (as in `did`). Previously, invalid weights gave
  `NaN` estimates or inconsistent results without an error.

* Empirical bootstrap: group effects and event times of a small cohort that
  is missing from some bootstrap draws now keep their estimates and get
  standard errors from the draws that contain the cohort; if it is missing
  from more than 10% of the draws, the standard errors are `NA` (#22).
  Before, such estimates were dropped from the results, estimate and all,
  if any single draw missed the cohort. The same applies to the QTT
  estimators. Also fixed: group-time standard errors from the empirical
  bootstrap were attached to the wrong (g,t) cells; they are now matched by
  group and time period.

* New `clustervars` argument for `pte_default()`, `pte()` (via
  `setup_pte()`) to cluster standard errors at a level above the unit, as
  in `did::att_gt()` (#6): one variable, constant within each unit. It
  applies to analytical standard errors (cluster-robust variance, including
  the pre-test), the multiplier bootstrap (one multiplier per cluster,
  applied to influence functions summed within clusters) and the empirical
  bootstrap (whole clusters are resampled), for group-time effects and all
  aggregations. `mboot2()` and `getSE()` gain a `cluster` argument.
  Unclustered results are unchanged.

* `setup_pte()` now stores `control_group` in the `pte_params` object, so
  later steps (the overall ATT for continuous treatments) use the requested
  comparison group instead of always the default. Continuous-treatment
  results (`process_dose_gt()`) now work with `base_period = "universal"`.

* Covariate formulas can now transform variables, e.g.
  `xformula = ~ factor(state) + I(x^2) + log(z)`, in `pte_default()` /
  `pte_attgt()`, `did_attgt()` and `covid_attgt()` (#5). Such terms failed
  with "object not found". Results for untransformed covariates are
  unchanged.

* An error while estimating one group-time cell now says which group and
  time period it is (in units of `tname`) (#17). With a continuous
  treatment, a cohort with no more treated units than parameters in the
  dose regression now gives a clear error instead of "something off with
  overall influence function".

* The empirical bootstrap no longer errors when a bootstrap draw contains no
  treated units, which happened often with small cohorts (#10). Such draws
  are dropped, with a warning that says how many, and standard errors use
  the remaining draws. This also applies to the QTT estimators in qte.

* `pte_default()` with `est_method = "dr"` no longer warns
  "non-integer #successes in a binomial glm!" on every call (#20). The
  overlap check now fits its logit propensity score with `quasibinomial()`
  and weights normalized to mean 1; fitted values, and so results, are
  unchanged.

* No more flood of warnings from the multiplier bootstrap with
  `base_period = "universal"` (#3). The sup-t critical value for uniform
  confidence bands is computed only when it is used (`cband = TRUE`), and
  only over parameters with a positive standard error, which leaves out the
  universal reference period (0 by construction). This removes one warning
  per bootstrap draw ("no non-missing arguments to max") and spurious
  "critical value ... somehow smaller" warnings. Results are unchanged.
  `mboot2()` gains a `cband` argument.

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
