# =============================================================================
# Title: Tests for sampling weights
# Description: Aggregated standard errors must not depend on the scale of the
#   sampling weights (issue #8) and should match did::att_gt().
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-14
# =============================================================================

test_that("weighted SEs are invariant to weight scale and match did", {
  set.seed(1)
  n <- 1000
  n_t <- 5
  G <- sample(c(0, 3, 4), n, replace = TRUE)
  d <- data.frame(id = rep(1:n, each = n_t), period = rep(1:n_t, n),
                  G = rep(G, each = n_t),
                  w = rep(runif(n, 0.5, 1.5) * 5e4, each = n_t))
  d$y <- rnorm(nrow(d)) + 1 * (d$G > 0 & d$period >= d$G)
  fit <- function(wname) {
    set.seed(2)
    suppressWarnings(pte_default(
      yname = "y", gname = "G", tname = "period", idname = "id", data = d,
      d_outcome = TRUE, est_method = "reg", weightsname = wname,
      cband = FALSE, biters = 999
    ))$overall_att
  }
  d$w_small <- d$w / 5e4
  big <- fit("w")
  small <- fit("w_small")
  expect_equal(big$overall.att, small$overall.att)
  expect_equal(big$overall.se, small$overall.se, tolerance = 1e-8)

  set.seed(2)
  cs <- did::att_gt(yname = "y", tname = "period", idname = "id", gname = "G",
                    data = d, weightsname = "w", est_method = "reg",
                    control_group = "notyettreated", biters = 999)
  cs_agg <- did::aggte(cs, type = "group", biters = 999)
  expect_equal(big$overall.att, cs_agg$overall.att, tolerance = 1e-8)
  # bootstrap SEs: allow for simulation noise
  expect_equal(big$overall.se, cs_agg$overall.se, tolerance = 0.1)
})

# overall_weights() checked sum(weights) == 1 with exact equality, which
# rejected valid weights off by floating-point rounding (~4e-16) with many
# timing groups (#1). This panel (13 timing groups, 14 periods, 147 units)
# triggered the error.
test_that("overall weights tolerate floating-point rounding", {
  set.seed(1)
  n <- 147
  n_periods <- 14
  grp <- sample(c(0, 2:14), n, replace = TRUE)
  d <- data.frame(id = rep(seq_len(n), each = n_periods),
                  period = rep(seq_len(n_periods), n),
                  grp = rep(grp, each = n_periods))
  d$y <- rnorm(nrow(d))
  res <- suppressWarnings(pte_default(
    yname = "y", gname = "grp", tname = "period", idname = "id", data = d,
    d_outcome = TRUE, est_method = "reg", biters = 0
  ))
  w <- overall_weights(res$att_gt)
  expect_equal(sum(w$overall_weight), 1)
  expect_true(all(w$overall_weight >= 0))
})

# pte_attgt()'s overlap check fitted a binomial glm() with weights that were
# never whole numbers, so every est_method = "dr" call warned "non-integer
# #successes in a binomial glm!" (#20).
test_that("doubly robust estimation does not warn about non-integer weights", {
  skip_if_not_installed("did")
  data <- did::mpdta
  set.seed(1)
  data$w <- rep(runif(500, 1, 100), each = 5)
  for (wname in list(NULL, "w")) {
    expect_no_warning(pte_default(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = data, d_outcome = TRUE,
      xformula = ~lpop, est_method = "dr", weightsname = wname, biters = 0
    ))
  }
})
