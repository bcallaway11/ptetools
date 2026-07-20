## Resubmission after archival

This package was archived on CRAN on 2026-07-16 for calling a deprecated
`BMisc` function, `rhs.vars()`. That call was replaced with the current,
non-deprecated `rhs_vars()` prior to archival, but the fix was never
submitted as a new CRAN release, so the archived 1.0.0 build still
contained the deprecated call. This submission (1.0.1) includes that fix.
To prevent a recurrence, the minimum required `BMisc` version has also
been bumped to 1.4.8, the version in which `rhs_vars()` and several other
`BMisc` functions `ptetools` relies on were introduced.

## Test environments

- Local Ubuntu 24.04.4 LTS, R 4.6.1:
    - All checks passed without issues.
- Github Actions
    - Windows-latest (R release)
    - Windows-latest (R devel)
    - macOS-latest (R release)
    - Ubuntu-latest (R release)
    - Ubuntu-latest (R devel)
    - All checks passed without issues.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional comments

* This is an update from the previously archived version (1.0.0); see
  above and NEWS.md for details.
* Besides the archival fix, this release adds a `pte_default()`
  convenience wrapper, support for repeated cross-sectional data, and
  print/summary/plot methods for empirical-bootstrap results. See
  NEWS.md for the full list of changes.
* Reverse dependency checks were run against all known dependent
  packages; no new issues were introduced by this release.
