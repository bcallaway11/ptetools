# Convert Data to Usable Format

Turns the rows selected for one group-time cell into the data that an
`attgt_fun` receives, and sets its class to include `gt_data_frame`.
Every `subset_fun` should finish by calling it.

Setup functions such as
[`setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md)
store their internal columns under dot-prefixed names (`.G`, `.id`,
`.period`, `.Y`), so they never overwrite the user's own columns.
`gt_data_frame()` copies these to the plain names `G`, `id`, `period`
and `Y` that `attgt_fun`s use. The `subset_fun` must already have added
`name` (`"pre"`/`"post"`) and the local treatment indicator `D`.

## Usage

``` r
gt_data_frame(data)
```

## Arguments

- data:

  rows of the full data (from a setup function) for one group-time cell,
  including the columns `.G`, `.id`, `.period`, `.Y`, `name` and `D`

## Value

`gt_data_frame` object, with columns `G`, `id`, `period`, `Y`, `name`
and `D` in addition to the columns of `data`
