## Coordinated submission with reverse dependencies

This release breaks the current CRAN versions of two reverse dependencies,
`contdid` (0.1.2) and `qte` (2.0.0). I maintain both, and updated
versions -- `contdid` 0.1.3 and `qte` 2.1.0, which require
`ptetools (>= 1.1.0)` and pass their checks against it -- are being
submitted at the same time as this release.

The breakage is intentional. `setup_pte()` used to overwrite the user's
group, time and outcome columns with internally recoded versions; it now
stores those under separate internal names (`.G`, `.period`, ...), so
users' own columns are never modified. The CRAN versions of `contdid` and
`qte` define their own subset functions that read the old column names.
In addition, one `qte` test hardcodes a quantile treatment effect whose
value changes because this release corrects how aggregated quantile
treatment effects are computed.

The third reverse dependency, `badcontrols`, passes its checks against
this release without changes.

## Test environments

- Local Ubuntu 26.04.1 LTS, R 4.5.2
- GitHub Actions
    - windows-latest (R release)
    - windows-latest (R devel)
    - macos-latest (R release)
    - ubuntu-latest (R release)
    - ubuntu-latest (R devel)
    - TODO: fill in results after the GitHub Actions run (release step 10)

## R CMD check results

`R CMD check --as-cran`:

0 errors | 0 warnings | 0 notes

(Locally, there is one NOTE that HTML validation and math rendering in
the manual were skipped because HTML Tidy and V8 are not installed on the
check machine; this is a property of the local setup, not the package.)

## Reverse dependencies

Checked all three CRAN reverse dependencies:

- `badcontrols` 1.0.1: OK.
- `contdid` 0.1.2: breaks (see above); fixed in 0.1.3, submitted together
  with this release.
- `qte` 2.0.0: breaks (see above); fixed in 2.1.0, submitted together
  with this release.

## Summary of changes

- New `clustervars` argument for clustered standard errors (analytical,
  multiplier bootstrap and empirical bootstrap).
- Sampling weights (`weightsname`) now work correctly throughout: several
  estimators and the empirical bootstrap previously ignored them.
- New diagnostics: `check_overlap()` and `unsupported_covariates()`;
  warnings from individual group-time cells are collected and reported
  once per call (`pte_cell_warning()`).
- Covariate formulas can transform variables, and users' data columns are
  no longer overwritten.
- Several bug fixes to standard errors, uniform confidence bands, and
  quantile treatment effect aggregation.

See NEWS.md for the full list of changes.
