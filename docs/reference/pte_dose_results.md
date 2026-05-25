# Class for Continuous Treatment Results

Class for holding results with a continuous treatment

## Usage

``` r
pte_dose_results(att_gt, dose, att_d = NULL, acrt_d = NULL, ptep)
```

## Arguments

- att_gt:

  attgt results

- dose:

  vector of doses

- att_d:

  ATT(d) for each value of `dose`

- acrt_d:

  ACRT(d) for each value of `dose`

- ptep:

  a `pte_params` object

## Value

a `pte_dose_results` object
