# Aggregates (g,t)-Specific Results

This is a slight edit of the aggte function from the `did` package.
Currently, it only provides aggregations for "overall" treatment effects
and event studies. It also will provide the weights directly which is
currently used for constructing aggregations based on distributions. The
other difference is that, `pte_aggte` provides inference results where
the only randomness is coming from the outcomes (not from the group
assignment nor from the covariates).

## Usage

``` r
pte_aggte(
  attgt,
  type = "overall",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  ...
)
```

## Arguments

- attgt:

  A group_time_att object to be aggregated

- type:

  The type of aggregation to be done. Default is "overall".

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

an `aggte_obj`
