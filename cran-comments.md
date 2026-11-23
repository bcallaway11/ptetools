## Coordinated submission with reverse dependencies

This release breaks the current CRAN versions of two reverse dependencies,
`contdid` (0.1.2) and `qte` (2.0.0). I maintain both, and updated
versions -- `contdid` 0.1.3 and `qte` 2.1.0, which require
`ptetools (>= 1.1.0)` and pass their checks against it -- are ready and
will be submitted as soon as this release is accepted.

The breaking change fixes an issue where it was possible for users to use
variable names that conflicted with internal column names and that were
natural choices for variable names.


## Test environments

- Local Ubuntu 26.04.1 LTS, R 4.5.2
- GitHub Actions
    - windows-latest (R release)
    - windows-latest (R devel)
    - macos-latest (R release)
    - ubuntu-latest (R release)
    - ubuntu-latest (R devel)
    - All five: Status OK (0 errors, 0 warnings, 0 notes)

## R CMD check results

`R CMD check --as-cran`:

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Checked all three CRAN reverse dependencies:

- `badcontrols` 1.0.1: OK.
- `contdid` 0.1.2: breaks (see above); fixed in 0.1.3, to be submitted as
  soon as this release is accepted.
- `qte` 2.0.0: breaks (see above); fixed in 2.1.0, to be submitted as
  soon as this release is accepted.

## Summary of changes

- New `clustervars` argument for clustered standard errors
- Sampling weights (`weightsname`) now work correctly throughout
- New diagnostics: `check_overlap()` and `unsupported_covariates()`
- Covariate formulas can transform variables, and users' data columns are
  no longer overwritten.
- Several bug fixes to standard errors, uniform confidence bands, and
  quantile treatment effect aggregation.

See NEWS.md for the full list of changes.
