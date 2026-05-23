# ptetools — Developer Notes

## Plotting: move to autoplot/plot S3 pattern

**Current state:** `ggpte()` and `ggpte_cont()` are standalone exported
functions in `R/ggpte.R`. These names are ad hoc and don't follow any
convention R users would recognize.

**Plan:** Replace with S3 methods following the `autoplot` + `plot` pattern:

- `autoplot.SomeClass(object, ...)` — builds and returns the ggplot object
  (user can add layers with `+`)
- `plot.SomeClass(x, ...)` — convenience wrapper that calls
  `print(autoplot(x, ...))`

This is standard practice (see `forecast`, `broom`). Since ptetools already
imports ggplot2, `autoplot` is available via `ggplot2::autoplot`.

**Classes to update:**

| Class | New method | Replaces |
|---|---|---|
| `pte_results` | `autoplot.pte_results`, `plot.pte_results` | `ggpte()` |
| `dose_obj` | `autoplot.dose_obj`, `plot.dose_obj` | `ggpte_cont()` |
| `pte_qtt` | `autoplot.pte_qtt`, `plot.pte_qtt` | (new) |

When adding `autoplot` methods, deprecate the corresponding `gg*` function
with `.Deprecated()` pointing to `autoplot()`.

**Starting point:** implement `autoplot.pte_qtt` and `plot.pte_qtt` first
(new class, no deprecation needed). The overall aggregation plot is the
priority: `probs` on x-axis, `qtt` on y-axis, ribbon for confidence band
when SEs are available, no ribbon when SEs are NA (i.e., when `biters = 0`).

---

## Remove tidyr dependency — replace pivot_wider with data.table::dcast

**Status:** Not yet done. Simple change, low risk.

`tidyr::pivot_wider` is called at exactly two sites in `R/attgt_functions.R`
(lines 35 and 173). ptetools already imports `data.table`, so no new dependency
is needed. The replacement pattern:

```r
# Replace:
gt_data_outcomes <- tidyr::pivot_wider(
  gt_data[, c("D", "id", "period", "name", "Y")],
  id_cols = c(id, D), names_from = name, values_from = Y
)

# With:
gt_data_outcomes <- data.table::dcast(
  data.table::as.data.table(gt_data[, c("D", "id", "period", "name", "Y")]),
  id + D ~ name, value.var = "Y"
)
```

After replacing both sites:
1. Remove `tidyr` from DESCRIPTION Imports.
2. Remove the `@import tidyr` tag from `R/imports.R` (if present).
3. Run `devtools::test()` to confirm nothing breaks.
4. Commit and push separately from any qte changes.

---

## biters = 0: skip bootstrap, return NA standard errors

**Motivation:** QTT estimation is computationally heavier than ATT because
there is no analytical variance — bootstrap is the only inference path.
A `biters = 0` option lets users get point estimates quickly (useful for
exploration, testing, sanity checks).

**Plan:**

1. Add an early-return check in `empirical_bootstrap()` and
   `qtt_empirical_bootstrap()`: when `ptep$biters == 0`, skip the bootstrap
   loop and return the result object with `se = NA`, `lower = NA`,
   `upper = NA`.
2. Fix the `1:biters` call at line 93 of `empirical_bootstrap.R` →
   `seq_len(biters)` for consistency (currently line 455 already uses
   `seq_len`).
3. Verify that `print` and `summary` methods handle NA SEs gracefully
   (expected: print NA, no crash).
4. Verify that `autoplot.pte_qtt` drops the confidence ribbon when SEs
   are NA rather than erroring.

**Note:** do not add a standalone `se` argument to user-facing functions
yet. `biters = 0` is the mechanism for now.

---

## Future idea: generic group support

Explore whether `ptetools` should support more generic group definitions beyond the current staggered-adoption-style `gname` setup. Useful follow-up questions:

- What group structures should be admissible besides first-treatment-period groups?
- Should setup functions validate and normalize arbitrary group labels before computing group-time effects?
- What aggregation weights are appropriate when groups are not treatment-timing cohorts?

Track future implementation work in a GitHub issue when GitHub authentication is available.

## Future idea: unequally spaced periods

Improve handling of time variables that are integer-valued but not consecutive, such as years 2001, 2004, 2007. Useful follow-up questions:

- Should `setup_pte()` internally map original periods to consecutive event-time indices while preserving original labels in output?
- Which subset and aggregation functions assume consecutive periods?
- How should anticipation and base-period logic behave when gaps between periods are unequal?

Track future implementation work in a GitHub issue when GitHub authentication is available.
