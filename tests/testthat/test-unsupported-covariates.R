# =============================================================================
# Title: Tests for unsupported_covariates()
# Description: unsupported_covariates() flags, per predicted unit, the
#   covariates dropped for collinearity in the fitting sample whose values
#   are not the fitting sample's linear combination of the kept columns
#   (#34). It is the check behind pte_attgt()'s dropped-covariate warning.
# Author: Brant Callaway
# Last update: 2026-11-20
# Date created: 2026-11-20
# =============================================================================

cov_data <- function(n = 20) {
  set.seed(1)
  D <- rep(0:1, each = n / 2)
  covmat <- cbind("(Intercept)" = 1, x = rnorm(n))
  list(D = D, covmat = covmat)
}

test_that("a dummy that is 1 only for some treated units is flagged", {
  cd <- cov_data()
  only_treated <- cd$D == 1 & seq_along(cd$D) %% 2 == 0
  covmat <- cbind(cd$covmat, region_c = as.numeric(only_treated))
  flags <- unsupported_covariates(covmat, cd$D)
  expect_equal(colnames(flags), "region_c")
  expect_equal(nrow(flags), sum(cd$D))
  expect_equal(flags[, "region_c"], only_treated[cd$D == 1])
})

test_that("columns equal among comparison units but not treated ones", {
  cd <- cov_data()
  z <- cd$covmat[, "x"]
  z[cd$D == 1][1:3] <- z[cd$D == 1][1:3] + 1
  covmat <- cbind(cd$covmat, z = z)
  flags <- unsupported_covariates(covmat, cd$D)
  expect_equal(ncol(flags), 1)
  expect_equal(sum(flags), 3)
})

test_that("redundant columns are not flagged", {
  cd <- cov_data()
  covmat <- cbind(cd$covmat, zero = 0, x_copy = 2 * cd$covmat[, "x"])
  flags <- unsupported_covariates(covmat, cd$D)
  expect_equal(ncol(flags), 0)
  expect_equal(nrow(flags), sum(cd$D))
  # no dropped columns at all
  expect_equal(ncol(unsupported_covariates(cd$covmat, cd$D)), 0)
})

test_that("keep_covs can be given, D can be reversed, inputs are checked", {
  cd <- cov_data()
  only_control <- cd$D == 0 & seq_along(cd$D) %% 2 == 0
  covmat <- cbind(cd$covmat, a = as.numeric(only_control))
  # fit on treated units, predict for comparison units
  flags <- unsupported_covariates(covmat, 1 - cd$D)
  expect_equal(flags[, "a"], only_control[cd$D == 0])
  expect_equal(unsupported_covariates(covmat, 1 - cd$D, keep_covs = 1:2),
               flags)
  expect_equal(unsupported_covariates(covmat, as.logical(1 - cd$D)), flags)
  expect_error(unsupported_covariates(covmat, cd$D[-1]), "one row per")
  expect_error(unsupported_covariates(covmat, rep(1, 20)), "no units")
})
