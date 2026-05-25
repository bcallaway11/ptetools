# summary.dose_obj

summarizes a `dose_obj` object

## Usage

``` r
# S3 method for class 'dose_obj'
summary(object, ...)
```

## Arguments

- object:

  an `dose_obj` object

- ...:

  extra arguments

## Value

a list containing the summary of a `dose_obj` object:

- `dose`: vector of dose values

- `overall_att`: overall ATT estimate

- `overall_att_se`: standard error of overall ATT estimate

- `overall_acrt`: overall ACRT estimate

- `overall_acrt_se`: standard error of overall ACRT estimate

- `att.d`: vector of ATT(d) estimates

- `att.d_se`: vector of standard errors for ATT(d) estimates

- `att.d_crit.val`: critical value for pointwise or uniform confidence
  interval for ATT(d)

- `acrt.d`: vector of ACRT(d) estimates

- `acrt.d_se`: vector of standard errors for ACRT(d) estimates

- `acrt.d_crit.val`: critical value for pointwise or uniform confidence
  interval for ACRT(d)

- `alp`: significance level

- `cband`: logical indicating whether to use simultaneous or pointwise
  confidence intervals

- `bstrap`: logical indicating whether to use bootstrap for critical
  value
