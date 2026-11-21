# Base Period of a Group-Time Cell

The base period for the cell of group `g` in period `tp`, as in
[`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html).
Post-treatment cells (`tp` at least `g - anticipation`, in calendar
time) and all cells with a universal base period use the last period `p`
with `p + anticipation < g`; pre-treatment cells with a varying base
period use the period just before `tp`. Used by
[`two_by_two_subset()`](https://github.com/bcallaway11/ptetools/reference/two_by_two_subset.md);
useful for custom subset functions.

## Usage

``` r
pte_base_period(data, g, tp, anticipation = 0, base_period = "varying")
```

## Arguments

- data:

  the full data from a setup function (e.g. `ptep$data`)

- g:

  the group (internal period index of first treatment)

- tp:

  the current period (internal period index)

- anticipation:

  anticipation, in units of the time variable

- base_period:

  `"varying"` or `"universal"`

## Value

the base period as an internal period index, or `NA` if group `g` has no
period early enough to serve as its base period
