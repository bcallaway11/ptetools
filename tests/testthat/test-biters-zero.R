# =============================================================================
# Title: Tests for biters = 0
# Description: biters = 0 skips the bootstrap (#14). With an influence
#   function it must match bstrap = FALSE; on the empirical-bootstrap path it
#   must return the same point estimates with NA standard errors. Summaries
#   and plots must handle the NA standard errors.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

run_mpdta <- function(...) {
  suppressWarnings(suppressMessages(pte_default(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = did::mpdta, d_outcome = TRUE, ...
  )))
}

test_that("biters = 0 matches bstrap = FALSE with an influence function", {
  skip_if_not_installed("did")
  a <- run_mpdta(biters = 0)
  b <- run_mpdta(bstrap = FALSE)
  expect_equal(a$att_gt$att, b$att_gt$att)
  expect_equal(a$att_gt$se, b$att_gt$se)
  expect_equal(a$overall_att$overall.se, b$overall_att$overall.se)
  expect_equal(a$event_study$se.egt, b$event_study$se.egt)
  expect_true(all(is.finite(a$overall_att$overall.se)))
  # pointwise intervals only
  expect_false(isTRUE(a$ptep$cband))
  expect_no_error(capture.output(summary(a), print(a)))
  expect_no_error(ggplot2::ggplot_build(autoplot(a)))
})

# local ATT without an influence function, so pte() uses the empirical
# bootstrap
noif_attgt <- function(gt_data, ...) {
  post <- gt_data$name == "post"
  attgt_noif(attgt = mean(gt_data$Y[gt_data$D == 1 & post]) -
               mean(gt_data$Y[gt_data$D == 0 & post]))
}

run_noif <- function(biters) {
  set.seed(1)
  suppressWarnings(suppressMessages(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = did::mpdta, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = noif_attgt,
    biters = biters
  )))
}

test_that("biters = 0 gives NA SEs on the empirical bootstrap path", {
  skip_if_not_installed("did")
  expect_message(
    suppressWarnings(pte(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = did::mpdta, setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset, attgt_fun = noif_attgt, biters = 0
    )),
    "biters = 0"
  )
  e0 <- run_noif(0)
  e5 <- run_noif(5)
  expect_s3_class(e0, "pte_emp_boot")
  # same point estimates as with bootstrap draws
  expect_equal(e0$overall_results$att, e5$overall_results$att)
  expect_equal(e0$dyn_results$att.e, e5$dyn_results$att.e)
  expect_equal(e0$attgt_results$att, e5$attgt_results$att)
  # no standard errors without the bootstrap
  expect_true(is.na(e0$overall_results$se))
  expect_true(all(is.na(e0$dyn_results$se)))
  expect_true(all(is.na(e0$group_results$se)))
  expect_true(all(is.na(e0$attgt_results$se)))
  expect_no_error(capture.output(summary(e0), print(e0)))
  expect_no_error(ggplot2::ggplot_build(autoplot(e0)))
})

test_that("invalid biters give clear errors", {
  skip_if_not_installed("did")
  expect_error(run_mpdta(biters = -1), "non-negative whole number")
  expect_error(run_mpdta(biters = 2.5), "non-negative whole number")
  expect_error(mboot2(matrix(rnorm(10)), biters = 0), "biters >= 1")
})
