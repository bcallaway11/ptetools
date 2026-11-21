# Print Overlap Diagnostics

Prints a short summary of
[`check_overlap()`](https://github.com/bcallaway11/ptetools/reference/check_overlap.md)
results: how many post-treatment cells have treated units without
comparison support, and the worst of them. The full table is the object
itself (a data frame).

## Usage

``` r
# S3 method for class 'pte_overlap'
print(x, n = 5, ...)
```

## Arguments

- x:

  a `pte_overlap` object

- n:

  number of cells to show

- ...:

  not used

## Value

`x`, invisibly
