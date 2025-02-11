## Test environments

- Local Ubuntu 24.04, R 4.4.1:
    - All checks passed without issues.
- Github Actions
    - Windows-latest (R release)
    - Windows-latest (R devel)
    - macOS-latest (R release)
    - Ubuntu-latest (R release)
    - Ubuntu-latest (R devel)
    - All checks passed without issues.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Additional comments

* Added return and \value to documentation for all functions
* Removed \dontrun tag from examples
* There are no missing references in the DESCRIPTION file