# =============================================================================
# Title: Tests for covariate formulas
# Description: xformula terms that transform variables (factor(), I(),
#   log()) must work in pte_attgt() and did_attgt() and give the same
#   estimates as precomputed columns (#5).
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

covariate_data <- function() {
  set.seed(1)
  n <- 300
  d <- expand.grid(id = seq_len(n), period = 1:4)
  d$state <- (d$id %% 3) + 1
  d$x <- rnorm(n)[d$id]
  d$grp <- ifelse(d$id <= 120, 3, 0)
  d$y <- rnorm(nrow(d)) + d$x + 0.5 * d$state +
    (d$grp > 0 & d$period >= 3)
  # the same transformations, precomputed
  d$state2 <- 1 * (d$state == 2)
  d$state3 <- 1 * (d$state == 3)
  d$x_sq <- d$x^2
  d$log_abs_x <- log(abs(d$x))
  d
}

run_cov <- function(xformula, attgt_fun = NULL) {
  d <- covariate_data()
  if (is.null(attgt_fun)) {
    res <- suppressWarnings(pte_default(
      yname = "y", gname = "grp", tname = "period", idname = "id", data = d,
      d_outcome = TRUE, xformula = xformula, biters = 0
    ))
  } else {
    res <- suppressWarnings(pte(
      yname = "y", gname = "grp", tname = "period", idname = "id", data = d,
      setup_pte_fun = setup_pte, subset_fun = two_by_two_subset,
      attgt_fun = attgt_fun, xformula = xformula, biters = 0
    ))
  }
  c(res$att_gt$att, res$att_gt$se)
}

test_that("transformed covariates match precomputed columns", {
  expect_equal(run_cov(~factor(state)), run_cov(~ state2 + state3))
  expect_equal(run_cov(~ x + I(x^2)), run_cov(~ x + x_sq))
  expect_equal(run_cov(~ log(abs(x))), run_cov(~log_abs_x))
})

test_that("did_attgt() handles transformed covariates", {
  expect_equal(run_cov(~factor(state), did_attgt),
               run_cov(~ state2 + state3, did_attgt))
})
