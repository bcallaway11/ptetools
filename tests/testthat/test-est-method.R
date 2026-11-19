# =============================================================================
# Title: Tests for est_method names
# Description: The doubly robust estimator is est_method = "aipw"; "dr" is a
#   deprecated alias that gives the same results with a once-per-session
#   warning, and other values give a clear error (#30).
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

run_method <- function(est_method, panel = TRUE) {
  pte_default(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = if (panel) "countyreal", data = did::mpdta, panel = panel,
    d_outcome = panel, xformula = ~lpop, est_method = est_method, biters = 0
  )
}

test_that("\"dr\" is a deprecated alias for \"aipw\"", {
  skip_if_not_installed("did")
  for (panel in c(TRUE, FALSE)) {
    aipw <- suppressWarnings(run_method("aipw", panel))
    pte_env$warned_dr <- NULL
    expect_warning(dr <- run_method("dr", panel), "deprecated")
    expect_equal(dr$att_gt$att, aipw$att_gt$att)
    expect_equal(dr$att_gt$se, aipw$att_gt$se)
    # the default is "aipw"
    default <- suppressWarnings(pte_default(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = if (panel) "countyreal", data = did::mpdta, panel = panel,
      d_outcome = panel, xformula = ~lpop, biters = 0
    ))
    expect_equal(default$att_gt$att, aipw$att_gt$att)
  }
  # warned once per session
  expect_no_warning(pte_est_method("dr"))
  pte_env$warned_dr <- NULL
})

test_that("an invalid est_method gives a clear error", {
  expect_error(pte_est_method("ipw"), "est_method must be")
  expect_error(pte_est_method(c("aipw", "reg")), "est_method must be")
})
