# =============================================================================
# Title: Tests for the multiplier bootstrap
# Description: mboot2() computes the sup-t critical value only when asked
#   (cband = TRUE) and only over parameters with a positive standard error,
#   so a parameter that is 0 by construction (a universal reference period)
#   no longer gives one warning per bootstrap draw (#3).
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

test_that("mboot2() leaves zero-variance parameters out of the sup-t", {
  set.seed(1)
  inf <- matrix(rnorm(500 * 3), 500, 3)
  with_zero <- cbind(inf, 0)

  set.seed(2)
  a <- mboot2(inf, biters = 200)
  set.seed(2)
  b <- expect_no_warning(mboot2(with_zero, biters = 200))
  expect_equal(b$crit_val, a$crit_val)
  expect_equal(b$boot_se[1:3], a$boot_se)
  expect_equal(b$boot_se[4], 0)
})

test_that("mboot2() skips the critical value when cband = FALSE", {
  set.seed(1)
  inf <- matrix(rnorm(500 * 2), 500, 2)
  set.seed(2)
  a <- mboot2(inf, biters = 100)
  set.seed(2)
  b <- mboot2(inf, biters = 100, cband = FALSE)
  expect_equal(b$boot_se, a$boot_se)
  expect_true(is.na(b$crit_val))
  # a single zero-variance parameter: no warnings, no critical value
  expect_no_warning(z <- mboot2(matrix(0, 100, 1), biters = 50))
  expect_true(is.na(z$crit_val))
})

test_that("a universal base period gives no bootstrap warnings", {
  set.seed(1)
  n <- 500
  treated <- rbinom(n, 1, 0.5)
  d <- expand.grid(id = seq_len(n), year = 2001:2005)
  d$grp <- ifelse(treated[d$id] == 1, 2003, 0)
  d$y <- rnorm(n)[d$id] + (d$grp > 0 & d$year >= 2003) + rnorm(nrow(d))
  for (cband in c(FALSE, TRUE)) {
    expect_no_warning(pte_default(
      yname = "y", gname = "grp", tname = "year", idname = "id", data = d,
      d_outcome = TRUE, est_method = "reg", base_period = "universal",
      cband = cband, biters = 100
    ))
  }
})
