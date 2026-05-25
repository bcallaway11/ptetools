# Weights for Overall Aggregation

A function that returns weights on (g,t)'s to deliver overall (averaged
across groups and time periods) treatment effect parameters

## Usage

``` r
overall_weights(attgt, balance_e = NULL, min_e = -Inf, max_e = Inf, ...)
```

## Arguments

- attgt:

  A group_time_att object to be aggregated

- balance_e:

  Drops groups that do not have at least `balance_e` periods of
  post-treatment data. This keeps the composition of groups constant
  across different event times in an event study. Default is NULL, in
  which case this is ignored.

- min_e:

  The minimum event time computed in the event study results. This is
  useful when there are a huge number of pre-treatment periods.

- max_e:

  The maximum event time computed in the event study results. This is
  useful when there are a huge number of post-treatment periods.

- ...:

  extra arguments

## Value

a data.frame containing columns:

- group: the group

- time.period: the time period

- overall_weight: the weight
