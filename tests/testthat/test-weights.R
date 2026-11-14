# =============================================================================
# Title: Tests for sampling weights
# Description: Aggregated standard errors must not depend on the scale of the
#   sampling weights (issue #8) and should match did::att_gt().
# Author: Brant Callaway
# Last update: 2026-11-14
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
