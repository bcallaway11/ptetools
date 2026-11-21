# Sanity-Check a Simultaneous Critical Value

Validates a bootstrapped simultaneous critical value, falling back to
the pointwise normal critical value if it is degenerate. Whether the
fallback happened is signaled through the `"cband"` attribute of the
returned value (`TRUE` if the simultaneous critical value is usable,
`FALSE` if we fell back to pointwise) – callers that need to downgrade
their own `cband` bookkeeping should read that attribute, since this
function only has access to `crit_val` and `alp`, not the caller's
`ptep`/`cband` state.

## Usage

``` r
crit_val_checks(crit_val, alp = 0.05)
```

## Arguments

- crit_val:

  a simultaneous critical value, possibly NA/Inf/degenerate

- alp:

  significance level

## Value

the (possibly corrected) critical value, with a `"cband"` logical
attribute
