# Class for Estimates across Groups and Time

Class that holds causal effect parameter estimates across timing groups
and time periods

## Usage

``` r
group_time_att(
  group,
  time.period,
  att,
  V_analytical,
  se,
  crit_val,
  inf_func,
  n,
  W,
  Wpval,
  cband,
  alp,
  ptep,
  extra_gt_returns
)
```

## Arguments

- group:

  numeric vector of groups for ATT(g,t)

- time.period:

  numeric vector of time periods for ATT(g,t)

- att:

  numeric vector containing the value of ATT(g,t) for corresponding
  group and time period

- V_analytical:

  analytical asymptotic variance matrix for ATT(g,t)'s

- se:

  numeric vector of standard errors

- crit_val:

  critical value (usually a critical value for conducting uniform
  inference)

- inf_func:

  matrix of influence function

- n:

  number of unique individuals

- W:

  Wald statistic for ATT(g,t) version of pre-test of parallel trends
  assumption

- Wpval:

  p-value for Wald pre-test of ATT(g,t) version of parallel trends
  assumption

- cband:

  logical indicating whether or not to report a confidence band

- alp:

  significance level

- ptep:

  `pte_params` object

- extra_gt_returns:

  list containing extra returns at the group-time level

## Value

object of class `group_time_att`
