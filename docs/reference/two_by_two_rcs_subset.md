# Two Period Two Group Repeated Cross Sections Subset

A function for computing a 2x2 subset for repeated cross sections data.
This is analogous to `two_by_two_subset`, but indexes observations by
rows rather than by panel ids.

## Usage

``` r
two_by_two_rcs_subset(
  data,
  g,
  tp,
  control_group = "notyettreated",
  anticipation = 0,
  base_period = "varying",
  ...
)
```

## Arguments

- data:

  the full dataset

- g:

  the current group

- tp:

  the current time period

- control_group:

  whether to use "notyettreated" (default) or "nevertreated"

- anticipation:

  the number of periods of anticipation (i.e., number of periods before
  the treatment happens where the treatment can "already" affect the
  outcome)

- base_period:

  The type of base period to use. This only affects the numeric value of
  results in pre-treatment periods. Results in post-treatment periods
  are not affected by this choice. The default is "varying", where the
  base period will "back up" to the immediately preceding period in
  pre-treatment periods. The other option is "universal" where the base
  period is fixed in pre-treatment periods to be the period right before
  the treatment starts. "Universal" is commonly used in
  difference-in-differences applications, but can be unnatural for other
  identification strategies.

- ...:

  extra arguments to get the subset correct

## Value

list that contains the following elements:

- `gt_data`: a `gt_data_frame` object that contains the correct subset
  of data

- `n1`: the number of observations in this subset

- `disidx`: a vector of the correct rows for this subset
