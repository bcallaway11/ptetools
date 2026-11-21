# Calendar Time of Internal Periods

[`setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md)
recodes time periods and groups to 1, ..., T (the internal columns
`.period` and `.G`). Anticipation and base periods are defined in units
of the time variable (calendar time), as in
[`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html),
so custom subset functions that compare periods with anticipation should
map them back with this function.

## Usage

``` r
pte_calendar_time(data, p)
```

## Arguments

- data:

  the full data from a setup function (e.g. `ptep$data`)

- p:

  internal period indices (e.g. values of `.period` or `.G`); 0, the
  never-treated group, maps to `NA`

## Value

the calendar times of `p`. For data whose periods were not recoded (e.g.
from
[`setup_pte_basic()`](https://github.com/bcallaway11/ptetools/reference/setup_pte_basic.md)),
`p` itself.
