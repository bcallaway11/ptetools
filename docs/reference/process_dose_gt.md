# Process Results with a Continuous Treatment

After computing results for each group and time period,
`process_dose_gt` combines/averages them into overall effects and/or
dose specific effects. This is generic code that can be used from
different ways of estimating causal effects across different timing
groups and periods in a previous step.

## Usage

``` r
process_dose_gt(gt_results, ptep, ...)
```

## Arguments

- gt_results:

  list of group-time specific results

- ptep:

  `pte_params` object

- ...:

  extra arguments

## Value

a `dose_obj` object
