# =============================================================================
# Title: Tests for row-order invariance and the multiplier bootstrap
# Description: Results must not depend on how the input rows are sorted
#   (issue #9), and the vectorized mboot2() must reproduce the draw-by-draw
#   multiplier bootstrap.
# Author: Brant Callaway
# Last update: 2026-11-14
# Date created: 2026-11-14
# =============================================================================

test_that("estimates and SEs do not depend on row order (#9)", {
  data(mpdta, package = "did")
  set.seed(1)
  mpdta$w <- ave(runif(nrow(mpdta), 0.5, 2), mpdta$countyreal, FUN = function(z) z[1])
  shuffled <- mpdta[sample(nrow(mpdta)), ]
  fit <- function(d) {
    set.seed(2)
    res <- suppressWarnings(pte_default(
      yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal",
      data = d, xformula = ~lpop, d_outcome = TRUE, est_method = "reg",
      weightsname = "w", biters = 200
    ))
    c(res$att_gt$att, res$att_gt$se, res$event_study$se.egt, res$overall_att$overall.se)
  }
  expect_equal(fit(shuffled), fit(mpdta), tolerance = 1e-10)
})

test_that("mboot2 matches the draw-by-draw multiplier bootstrap", {
  set.seed(3)
  inffunc <- matrix(rnorm(300 * 4), 300, 4)
  set.seed(4)
  fast <- mboot2(inffunc, biters = 250)
  set.seed(4)
  bres <- t(vapply(1:250, function(b) {
    Ub <- sample(c(-1, 1), size = 300, replace = TRUE)
    sqrt(300) * colMeans(Ub * inffunc)
  }, numeric(4)))
  iqr <- function(b) (quantile(b, .75, type = 1) - quantile(b, .25, type = 1)) / (qnorm(.75) - qnorm(.25))
  expect_equal(fast$boot_se, apply(bres, 2, iqr) / sqrt(300), tolerance = 1e-12, ignore_attr = TRUE)
})
