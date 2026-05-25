# Convert Data to Usable Format

Checks and converts data to satisfy criteria to be used in internal
`ptetools` functions. In particular, the function takes in a data.frame,
checks if it has the right columns to be used to calculate a group-time
average treatment effect, and sets the class of the data.frame to
include `gt_data_frame`

## Usage

``` r
gt_data_frame(data)
```

## Arguments

- data:

  data that will be checked to see if has right format for computing
  group-time average treatment effects

## Value

`gt_data_frame` object
