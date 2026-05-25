# ptetools Release Checklist

Target version: **1.0.1** (already set in DESCRIPTION; confirm before release)

---

## R CMD CHECK — Fix Warnings and Notes

These are blockers for CRAN.

### Warnings (must fix)

- [x] **Deprecated `ggpte()` in examples** — replaced with
  `ggplot2::autoplot()` in both example blocks in `R/pte.R`.

- [x] **Undocumented arguments**
  - `pte.Rd`: added `@param aggregation_fun` to `R/pte.R`.
  - `pte_emp_boot.Rd`: added `@param ptep` to `R/classes.R`.
  - `qtt_empirical_bootstrap.Rd`: added `@param aggregation_fun` to
    `panel_empirical_bootstrap()` in `R/empirical_bootstrap.R`; inherited
    by `qtt_empirical_bootstrap()` via `@inheritParams`.

- [x] **`plot.pte_qtt.Rd` undocumented arguments** (`cband`, `plot_probs`,
  `plot_ci`) — added `@param` entries to `plot.pte_qtt` roxygen block in
  `R/ggpte.R`.

### Notes (should fix)

- [x] **`utils::globalVariables` incomplete** — added `est`, `crit`, `se`,
  `probs`, `qtt` to `R/zzz.R`.

- [x] **`.devcontainer` in package tree** — directory deleted; removed
  `^\.devcontainer$` from `.Rbuildignore`.

- [x] **`data.table` not declared in DESCRIPTION** — added `data.table` to
  `Imports` in `DESCRIPTION`.

---

## GitHub Actions — Update to Current Workflow Standards

The existing `check-package.yml` is outdated (triggers on every push, uses
old action versions, includes a coverage step we don't want).

- [x] Replaced `check-package.yml` with `R-CMD-check.yml` — PR +
  `workflow_dispatch`, 5 platforms, `r-lib/actions/check-r-package@v2`,
  no separate test step, no coverage.

- [x] Added `build-check.yml` — PR + `workflow_dispatch`; builds README,
  vignettes, and pkgdown site.

- [x] Replaced `revdep-check-test.yml` with `revdep-check.yml` — added PR
  trigger, updated all actions to `checkout@v6` / `upload-artifact@v7`.

- [x] Deleted `update-citation.yml` — citations updated manually.

---

## Code Cleanup

- [x] **`R/imputation_functions.R` stub** — replaced `browser()` with
  `stop("not yet implemented")`, added file header and stub comment noting
  it requires `fixest` and is excluded from the build.

- [x] **Drop `tidyr` dependency** — already done; both `pivot_wider` calls
  in `R/attgt_functions.R` use `data.table::dcast` and `tidyr` is absent
  from `DESCRIPTION` and `NAMESPACE`.

---

## Documentation and Metadata

- [ ] **Build README** — run `quarto render README.qmd` to re-render
  `README.qmd` → `README.md` and confirm output is current.

- [ ] **Build pkgdown site** — run `pkgdown::build_site()` and push the
  updated `docs/` folder.

- [x] **Review and update `NEWS.md`** — added bullet summarising session
  changes (covid_data, workflow updates, doc fixes, tidyr removal).

- [ ] **Update `inst/CITATION` and `CITATION.cff`** if the version number
  changes. Do this manually (no CI workflow).

- [ ] **Check README** — make sure examples still run, links are live, and
  the version badge reflects the release.

---

## Final Checks

- [ ] `devtools::document()` — regenerate `NAMESPACE` and `man/` and commit
  any changes.
- [ ] `devtools::test()` — all tests pass.
- [ ] `rcmdcheck::rcmdcheck()` — 0 errors, 0 warnings, notes addressed.
- [ ] `revdeplite::revdeplite()` — run locally before submission to catch
  any breakage in downstream packages.
- [ ] `devtools::build()` — clean tarball builds without warnings.
- [ ] Confirm version number in `DESCRIPTION` is correct for release.
- [ ] Tag the release commit in git.
- [ ] Submit to CRAN (or `devtools::submit_cran()`).
