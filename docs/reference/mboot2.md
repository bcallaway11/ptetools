# Multiplier Bootstrap

Function for using multiplier bootstrap to conduct inference

## Usage

``` r
mboot2(inffunc, biters = 1000, alp = 0.05)
```

## Arguments

- inffunc:

  influence function matrix

- biters:

  number of bootstrap iterations; default is 100

- alp:

  significance level; default is 0.05

## Value

list with the following elements:

- `boot_se`: bootstrap standard errors

- `crit_val`: critical value for uniform confidence bands
