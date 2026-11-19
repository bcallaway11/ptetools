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

# A cohort missing from a few bootstrap draws keeps its estimates and gets
# SEs from the draws that contain it; one missing from more than 10% of the
# draws keeps its estimates with NA SEs (#22). Before, it was dropped,
# estimates and all, if any single draw missed it.
small_cohort_data <- function(k) {
  set.seed(5)
  n <- 400
  n_periods <- 5
  grp <- c(rep(4, k), rep(3, 120), rep(0, n - 120 - k))
  d <- data.frame(id = rep(seq_len(n), each = n_periods),
                  period = rep(seq_len(n_periods), n),
                  grp = rep(grp, each = n_periods))
  d$y <- rnorm(nrow(d)) + (d$grp > 0 & d$period >= d$grp)
  d
}

run_small <- function(k) {
  set.seed(1)
  suppressWarnings(suppressMessages(pte(
    yname = "y", gname = "grp", tname = "period", idname = "id",
    data = small_cohort_data(k), setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = pte_attgt, xformula = ~1,
    d_outcome = TRUE, boot_type = "empirical", biters = 100
  )))
}

test_that("small cohorts keep their estimates and get SEs when possible", {
  # a cohort of 4 units is missing from about 2% of draws
  res4 <- run_small(4)
  expect_true(4 %in% res4$group_results$group)
  expect_true(is.finite(res4$group_results$se[res4$group_results$group == 4]))
  expect_true(-2 %in% res4$dyn_results$e)

  # a cohort of 1 unit is missing from about 37% of draws: NA SEs
  res1 <- run_small(1)
  expect_true(4 %in% res1$group_results$group)
  expect_true(is.na(res1$group_results$se[res1$group_results$group == 4]))
  expect_true(is.finite(res1$group_results$se[res1$group_results$group == 3]))
})

test_that("group-time SEs are matched to the right cells", {
  # with one cohort, the event-study SE at e and the ATT(g,t) SE of the cell
  # with t - g = e come from the same draws, so they must be equal
  res <- run_small(4)
  att <- res$attgt_results
  cell <- att$group == 3 & att$time.period == 5
  expect_equal(att$se[cell], res$dyn_results$se[res$dyn_results$e == 2])
})

# pte_qtt output (#26): summary() returns an object without printing,
# print() shows the overall, dynamic and group tables once, and autoplot()
# draws the group QTTs.
test_that("pte_qtt summary, print and group plot", {
  # a minimal pte_qtt object: two quantiles, two event times, two groups
  mk <- function(keys) {
    df <- expand.grid(k = keys, probs = c(0.25, 0.5))
    df$qtt <- seq_len(nrow(df)) / 10
    df$se <- 0.05
    df$lower_pw <- df$qtt - 0.1
    df$upper_pw <- df$qtt + 0.1
    df$lower_ub <- df$qtt - 0.15
    df$upper_ub <- df$qtt + 0.15
    df
  }
  dyn <- mk(c(1, 0, -1))
  names(dyn)[1] <- "e"
  grp <- mk(c(2004, 2006))
  names(grp)[1] <- "group"
  ovl <- mk(1)[, -1]
  q <- pte_qtt(overall = ovl, dynamic = dyn, group = grp,
               ptep = list(alp = 0.05, cband = TRUE))

  expect_silent(s <- summary(q))
  expect_s3_class(s, "summary.pte_qtt")
  expect_equal(s$dynamic$`Event Time`, c(-1, 0, 1))
  out <- capture.output(print(q))
  expect_equal(sum(grepl("Overall QTT curve", out)), 1)
  expect_true(any(grepl("Group effects \\(quantile 0.5\\)", out)))
  expect_true(any(grepl("Simult.", out)))
  expect_equal(nrow(summary(q, probs = c(0.25, 0.5))$group), 4)
  expect_error(summary(q, probs = 0.42), "not among the estimated quantiles")

  for (pp in list(0.5, c(0.25, 0.5))) {
    expect_no_error(ggplot2::ggplot_build(
      autoplot(q, type = "group", plot_probs = pp)
    ))
  }
})
