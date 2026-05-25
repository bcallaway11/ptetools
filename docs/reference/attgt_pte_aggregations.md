# Aggregate Group-Time Average Treatment Effects

Aggregate group-time average treatment effects into overall, group, and
dynamic effects. This function is only used for (i) computing standard
errors using the empirical bootstrap, and (ii) combining distributions
at the (g,t) level

## Usage

``` r
attgt_pte_aggregations(attgt.list, ptep)
```

## Arguments

- attgt.list:

  list of attgt results from `compute.pte`

- ptep:

  `pte_params` object

## Value

`pte_emp_boot` object
