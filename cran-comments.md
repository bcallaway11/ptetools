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

* Removed Logo field from DESCRIPTION file which was generating a NOTE.
* Removed inference tests and don't run some examples to remove NOTE
   related to overall check time > 10 minutes.