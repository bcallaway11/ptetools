# Keep All Pre-Treatment Subset

A function that takes an original data set and keeps all data for all
groups that are not-yet-treated by period `tp` as well as for group `g`.

In particular, this keeps more data than functions like `two_by_two`
subset that use a fixed base period.

A main use case for this function is the interactive fixed effects
approach proposed in Callaway and Tsyawo (2023).

## Usage

``` r
keep_all_pretreatment_subset(data, g, tp, ...)
```

## Arguments

- data:

  the full dataset

- g:

  the current group

- tp:

  the current time period

- ...:

  additional arguments

## Value

list that contains the following elements:

- `gt_data`: a `gt_data_frame` object that contains the correct subset
  of data

- `n1`: the number of observations in this subset

- `disidx`: a vector of the correct ids for this subset
