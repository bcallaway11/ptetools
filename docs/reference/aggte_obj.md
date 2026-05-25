# Aggregated Treatment Effects Class

Objects of this class hold results on aggregated group-time average
treatment effects. This is derived from the AGGTEobj class in the `did`
package.

An object for holding aggregated treatment effect parameters.

## Usage

``` r
aggte_obj(
  overall.att = NULL,
  overall.se = NULL,
  type = "simple",
  egt = NULL,
  att.egt = NULL,
  se.egt = NULL,
  crit.val.egt = NULL,
  inf.function = NULL,
  min_e = NULL,
  max_e = NULL,
  balance_e = NULL,
  DIDparams = NULL
)
```

## Arguments

- overall.att:

  The estimated overall ATT

- overall.se:

  Standard error for overall ATT

- type:

  The type of aggregation to be done. Default is "overall".

- egt:

  Holds the length of exposure (for dynamic effects), the group (for
  selective treatment timing), or the time period (for calendar time
  effects)

- att.egt:

  The ATT specific to egt

- se.egt:

  The standard error specific to egt

- crit.val.egt:

  A critical value for computing uniform confidence bands for dynamic
  effects, selective treatment timing, or time period effects.

- inf.function:

  The influence function of the chosen aggregated parameters

- min_e:

  The minimum event time computed in the event study results. This is
  useful when there are a huge number of pre-treatment periods.

- max_e:

  The maximum event time computed in the event study results. This is
  useful when there are a huge number of post-treatment periods.

- balance_e:

  Drops groups that do not have at least `balance_e` periods of
  post-treatment data. This keeps the composition of groups constant
  across different event times in an event study. Default is NULL, in
  which case this is ignored.

- DIDparams:

  A DIDparams object

## Value

an aggte_obj
