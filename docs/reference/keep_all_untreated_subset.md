# Keep All Untreated Subset

A function that takes an original data set and keeps all pre-treatment
data for all groups. For group g, it also includes data for the current
period.

Also, note that if `tp` is still a pre-treatment period for group g,
then periods after `tp` will also be dropped for group g. This is a
design choice and is useful especially for estimating placebo group-time
average treatment effects in pre-treatment periods.

A main use case for this function is to compute ATT(g,t)'s using a
global estimation strategy such as imputation in Gardner (2022).

## Usage

``` r
keep_all_untreated_subset(data, g, tp, ...)
```

## Arguments

- data:

  the full dataset

- g:

  the current group

- tp:

  the current time period

- ...:

  extra arguments to get the subset correct

## Value

list that contains the following elements:

- `gt_data`: a `gt_data_frame` object that contains the correct subset
  of data

- `n1`: the number of observations in this subset

- `disidx`: a vector of the correct ids for this subset
