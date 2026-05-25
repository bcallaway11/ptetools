# Basic Setup Function

This is a lightweight (example) function for how to setup the data to be
used in the `ptetools` package.

`setup_pte_basic` takes in information about the structure of `data` and
returns a `pte_params` object. The key piece of information that is
computed by this function is the list of groups and list of time periods
where ATT(g,t) should be computed. In particular, this function omits
the never-treated group but includes all other groups and drops the
first time period. This setup is basically geared towards the 2x2 case —
i.e., where ATT could be identified with two periods, a treated and
untreated group, and the first period being pre-treatment for both
groups. This is the relevant case for DID, but is also relevant for
other cases as well. However, for example, if more pre-treatment periods
were needed, then this function should be replaced by something else.

For code that is written with the idea of being easy-to-use by other
researchers, this is a good place to do some error handling / checking
that the data is in the correct format, etc.

## Usage

``` r
setup_pte_basic(
  yname,
  gname,
  tname,
  idname = NULL,
  data,
  panel = TRUE,
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
  gt_type = "att",
  ret_quantile = 0.5,
  probs = NULL,
  biters = 100,
  cl = 1,
  call = NULL,
  ...
)
```

## Arguments

- yname:

  Name of outcome in `data`

- gname:

  Name of group in `data`

- tname:

  Name of time period in `data`

- idname:

  Name of id in `data`

- data:

  balanced panel or repeated cross sections data

- panel:

  Whether the data are panel data. The default is TRUE. Set to FALSE for
  repeated cross sections.

- cband:

  whether or not to report a uniform (instead of pointwise) confidence
  band (default is TRUE)

- alp:

  significance level; default is 0.05

- boot_type:

  which type of bootstrap to use

- gt_type:

  which type of group-time effects are computed. The default is "att".
  Different estimation strategies can implement their own choices for
  `gt_type`

- ret_quantile:

  For functions that compute quantile treatment effects, this is a
  specific quantile at which to report results, e.g.,
  `ret_quantile = 0.5` will return that the qte at the median.

- probs:

  For `gt_type = "qtt"`, a numeric vector of quantile levels at which to
  evaluate the QTT curve (e.g., `seq(0.05, 0.95, 0.05)`). Defaults to
  `seq(0.05, 0.95, 0.05)` when NULL.

- biters:

  number of bootstrap iterations; default is 100

- cl:

  number of clusters to be used when bootstrapping; default is 1

- call:

  keeps track of through the `call` from external functions/packages

- ...:

  additional arguments

## Value

`pte_params` object
