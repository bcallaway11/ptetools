# =============================================================================
# Title: Tests for overlap diagnostics
# Description: check_overlap() reports, for the same group-time cells as
#   pte_default(), the share of treated units outside the comparison
#   group's covariate range and with propensity scores near 1 (#28).
# Author: Brant Callaway
# Last update: 2026-11-20
# Date created: 2026-11-20
# =============================================================================

# one treated cohort (G = 3) and never-treated units, four periods
overlap_data <- function(n = 400, seed = 1) {
  set.seed(seed)
  treated <- rep(c(TRUE, FALSE), length.out = n)
  d <- expand.grid(id = seq_len(n), period = 1:4)
  d$G <- ifelse(treated[d$id], 3, 0)
  d$x <- rnorm(n)[d$id]
  d$region <- sample(1:4, n, replace = TRUE)[d$id]
  d$w <- 1
  d$y <- d$x + rnorm(nrow(d))
  d
}

run_overlap <- function(d, ...) {
  check_overlap(yname = "y", gname = "G", tname = "period", idname = "id",
                data = d, ...)
}

test_that("cells match those of pte_default()", {
  d <- did::mpdta
  for (cg in c("notyettreated", "nevertreated")) {
    for (bp in c("varying", "universal")) {
      ov <- check_overlap("lemp", "first.treat", "year", "countyreal", d,
                          xformula = ~lpop, control_group = cg,
                          base_period = bp)
      est <- suppressWarnings(pte_default(
        "lemp", "first.treat", "year", "countyreal", data = d,
        xformula = ~lpop, control_group = cg, base_period = bp, biters = 0
      ))
      # universal base period: reference cells have no diagnostics
      keep <- !(bp == "universal" & est$att_gt$att == 0)
      expect_equal(paste(ov$group, ov$time.period),
                   paste(est$att_gt$group, est$att_gt$t)[keep])
    }
  }
  # numbers of units in a cell
  ov <- check_overlap("lemp", "first.treat", "year", "countyreal", d,
                      xformula = ~lpop, control_group = "nevertreated")
  first <- d[d$year == 2003, ]
  expect_true(all(ov$n_treated[ov$group == 2004] ==
                    sum(first$first.treat == 2004)))
  expect_true(all(ov$n_comparison == sum(first$first.treat == 0)))
})

test_that("a region with only treated units is outside the support", {
  d <- overlap_data()
  # region 5: treated units only
  move <- d$G == 3 & d$id %% 10 == 1
  d$region[move] <- 5
  ov <- run_overlap(d, xformula = ~ factor(region))
  share <- mean(d$region[d$period == 1 & d$G == 3] == 5)
  expect_equal(ov$outside_range, rep(share, nrow(ov)))
  expect_equal(unique(ov$worst_covariate), "factor(region)5")
})

test_that("treated units above the comparison maximum are counted", {
  d <- overlap_data()
  treated_ids <- unique(d$id[d$G == 3])
  top <- treated_ids[seq_len(0.2 * length(treated_ids))]
  d$x[d$id %in% top] <- max(d$x) + 50
  ov <- run_overlap(d, xformula = ~x)
  expect_equal(ov$outside_range, rep(0.2, nrow(ov)))
  # heavier weights on those units raise the weighted share
  d$w[d$id %in% top] <- 4
  ov_w <- run_overlap(d, xformula = ~x, weightsname = "w")
  expect_equal(ov_w$outside_range, ov$outside_range)
  expect_equal(ov_w$outside_range_w, rep(0.8 / (0.8 + 0.8), nrow(ov)))
  # far outside the comparison range: propensity scores near 1
  expect_true(all(ov$pscore_above_0.9 > 0))
})

test_that("full overlap gives no flags", {
  d <- overlap_data()
  ov <- run_overlap(d, xformula = ~x)
  # with a normal covariate a few treated units can sit just outside the
  # comparison range; none have propensity scores near 1
  expect_true(all(ov$outside_range < 0.02))
  expect_true(all(ov$pscore_above_0.9 == 0))
  expect_output(print(run_overlap(d)), "No covariates to check")
})

test_that("a cohort with too few treated units has no propensity score", {
  d <- overlap_data()
  keep_treated <- unique(d$id[d$G == 3])[1:2]
  d <- d[d$G == 0 | d$id %in% keep_treated, ]
  ov <- run_overlap(d, xformula = ~ x + factor(region))
  expect_false(any(ov$pscore_ok))
  expect_true(all(is.na(ov$pscore_above_0.9)))
  expect_true(all(!is.na(ov$outside_range)))
})

test_that("the lagged outcome is checked with lagged_outcome_cov = TRUE", {
  d <- overlap_data()
  # treated units' outcomes far above the comparison units' in periods 1-2
  bump <- d$G == 3 & d$period <= 2 & d$id %% 4 == 1
  d$y[bump] <- d$y[bump] + 100
  ov <- run_overlap(d, xformula = ~x, lagged_outcome_cov = TRUE)
  expect_true(all(ov$outside_range[ov$post] > 0.4))
  expect_equal(unique(ov$worst_covariate[ov$post]), "pre")
  expect_true(all(run_overlap(d, xformula = ~x)$outside_range < 0.02))
})

test_that("invalid arguments give clear errors", {
  d <- overlap_data()
  expect_error(run_overlap(d, panel = FALSE), "panel data only")
  expect_error(run_overlap(d, pscore_thresholds = 1), "strictly between")
})

test_that("the switch to regression adjustment says why", {
  d <- overlap_data()
  warnings_from <- function(data, xformula) {
    testthat::capture_warnings(pte_default(
      "y", "G", "period", "id", data = data, xformula = xformula, biters = 0
    ))
  }
  # two treated units (fewer than the covariates), far from the comparison
  # units so that the logit separates
  treated_ids <- unique(d$id[d$G == 3])
  d_small <- d[d$G == 0 | d$id %in% treated_ids[1:2], ]
  d_small$x[d_small$G == 3] <- max(d_small$x) + 50
  w <- warnings_from(d_small, ~ x + factor(region))
  expect_true(length(w) > 0)
  expect_match(w, "too few treated units", all = FALSE)
  # many treated units, 20% of them far outside the comparison range
  d$x[d$id %in% treated_ids[1:40]] <- max(d$x) + 50
  w <- warnings_from(d, ~x)
  expect_true(length(w) > 0)
  expect_match(w, "limited overlap", all = FALSE)
})
