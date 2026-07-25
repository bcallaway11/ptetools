# =============================================================================
# Title: Regression test for autoplot.dose_obj() attribute bug
# Description: A critical value carrying a stray "cband" (or other)
#   attribute broke data.frame() recycling in autoplot.dose_obj(); guards
#   against reintroducing an attributed scalar into att.d_crit.val /
#   acrt.d_crit.val. See dev/NOTES.md.
# Author: Brant Callaway
# Last update: 2026-07-25
# Date created: 2026-07-25
# =============================================================================

test_that("autoplot.dose_obj works when crit vals carry stray attributes", {
  dose <- seq(0.1, 0.9, length.out = 5)

  # deliberately attach attributes the way crit_val_checks()/quantile() do,
  # to make sure autoplot.dose_obj() (and the data.frame() call inside it)
  # tolerates this even if it creeps back in upstream
  attributed_crit <- structure(1.96, names = "95%", cband = TRUE)

  obj <- dose_obj(
    dose = dose,
    att.d = rnorm(5),
    att.d_se = runif(5, 0.1, 0.5),
    att.d_crit.val = attributed_crit,
    acrt.d = rnorm(5),
    acrt.d_se = runif(5, 0.1, 0.5),
    acrt.d_crit.val = attributed_crit
  )

  expect_s3_class(ggplot2::autoplot(obj, type = "att"), "ggplot")
  expect_s3_class(ggplot2::autoplot(obj, type = "acrt"), "ggplot")
})
