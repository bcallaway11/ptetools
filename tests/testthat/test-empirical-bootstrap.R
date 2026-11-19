# =============================================================================
# Title: Tests for the empirical bootstrap
# Description: Bootstrap draws that contain no treated units are dropped with
#   a warning instead of erroring (#10).
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

test_that("draws without treated units are dropped, not an error", {
  # one cohort of 2 treated units among 300: a draw misses it with
  # probability about exp(-2), so 50 draws almost surely include some
  set.seed(123)
  n <- 300
  n_periods <- 4
  d <- data.frame(id = rep(seq_len(n), each = n_periods),
                  period = rep(seq_len(n_periods), n))
  d$grp <- ifelse(d$id <= 2, 3, 0)
  d$y <- rnorm(nrow(d)) + d$id / n

  set.seed(1)
  warnings <- character()
  res <- withCallingHandlers(
    suppressMessages(pte(
      yname = "y", gname = "grp", tname = "period", idname = "id", data = d,
      setup_pte_fun = setup_pte, subset_fun = two_by_two_subset,
      attgt_fun = pte_attgt, xformula = ~1, d_outcome = TRUE,
      boot_type = "empirical", biters = 50
    )),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(res, "pte_emp_boot")
  expect_true(any(grepl("bootstrap draws had no treated units", warnings)))
  expect_true(is.finite(res$overall_results$se) && res$overall_results$se > 0)
})
