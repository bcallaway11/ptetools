# =============================================================================
# Title: Tests for the dose spline basis
# Description: Regression tests for dose_spline_basis(), which builds the
#   B-spline basis that process_dose_gt() uses for the influence functions of
#   ATT(d) and ACRT(d). It must use the boundary knots the dose regressions
#   were fitted with (ptetools #7, contdid #16).
# Author: Brant Callaway
# Last update: 2026-11-18
# Date created: 2026-11-18
# =============================================================================

set.seed(1)
dose <- runif(500)
dvals <- unname(quantile(dose, probs = seq(.1, .99, .01)))
knots <- unname(quantile(dose, probs = c(1 / 3, 2 / 3)))
boundary_knots <- range(dose)

test_that("dose_spline_basis() uses the supplied boundary knots", {
  basis <- dose_spline_basis(dvals, degree = 3, knots = knots,
                             boundary_knots = boundary_knots)
  fitted_basis <- splines2::bSpline(dvals, degree = 3, knots = knots,
                                    Boundary.knots = boundary_knots)
  fitted_deriv <- splines2::dbs(dvals, degree = 3, knots = knots,
                                Boundary.knots = boundary_knots)
  expect_equal(unname(basis$bs_grid), unname(cbind(1, fitted_basis)))
  expect_equal(unname(basis$bs_deriv), unname(cbind(0, fitted_deriv)))

  # the default basis (boundary knots at range(dvals)) is genuinely different
  # here, so the test would catch a regression to the old behavior
  default_basis <- splines2::bSpline(dvals, degree = 3, knots = knots)
  expect_gt(max(abs(basis$bs_grid[, -1] - default_basis)), 0.1)
})

test_that("dose_spline_basis() falls back to range(dvals) by default", {
  basis <- dose_spline_basis(dvals, degree = 3, knots = knots)
  default_basis <- splines2::bSpline(dvals, degree = 3, knots = knots)
  expect_equal(unname(basis$bs_grid), unname(cbind(1, default_basis)))
})

test_that("cubic basis with fitted boundary knots reproduces a cubic fit", {
  # with degree 3 and no interior knots, the B-spline basis spans the cubics,
  # so predictions at dvals from a regression on the basis must match a raw
  # cubic polynomial fit -- but only if both use the same basis
  y <- 1 + 2 * dose^2 + rnorm(length(dose), sd = 0.1)
  fit_basis <- splines2::bSpline(dose, degree = 3, knots = NULL,
                                 Boundary.knots = boundary_knots)
  bet <- coef(lm(y ~ fit_basis))
  basis <- dose_spline_basis(dvals, degree = 3, knots = NULL,
                             boundary_knots = boundary_knots)
  poly_fit <- lm(y ~ poly(dose, 3, raw = TRUE))
  expected <- predict(poly_fit, newdata = data.frame(dose = dvals))
  expect_equal(as.numeric(basis$bs_grid %*% bet), unname(expected),
               tolerance = 1e-8)
  expect_equal(as.numeric(basis$bs_deriv %*% bet),
               unname(coef(poly_fit)[2] + 2 * coef(poly_fit)[3] * dvals +
                        3 * coef(poly_fit)[4] * dvals^2),
               tolerance = 1e-6)
})
