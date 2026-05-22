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
