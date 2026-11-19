# =============================================================================
# Title: Tests for exact QTT aggregation
# Description: qtt_pte_aggregations() mixes the cells' CDFs exactly, from
#   every support point, rather than on a 1,000-point grid of the pooled
#   outcome (#25). Checked against a brute-force mixture, and integer weights
#   must give the same QTTs as the equivalent row-replicated data.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

# brute force: evaluate every cell's CDF at every pooled support point
brute_quantile <- function(cdfs, weights, probs) {
  x <- sort(unique(unlist(lapply(cdfs, stats::knots))))
  fx <- Reduce(`+`, Map(function(f, w) w * f(x), cdfs, weights))
  vapply(probs, function(tau) x[which(fx >= tau - 1e-12)[1]], numeric(1))
}

test_that("mixtures of weighted CDFs are exact", {
  set.seed(1)
  cdfs <- list(
    BMisc::weighted_ecdf(rnorm(50), weights = runif(50)),
    BMisc::weighted_ecdf(rnorm(80, 1), weights = runif(80)),
    BMisc::weighted_ecdf(round(rnorm(30), 1))  # ties
  )
  w <- c(0.5, 0.3, 0.2)
  probs <- c(0.01, seq(0.05, 0.95, 0.05), 0.99, 1)
  mix <- mix_cdf(lapply(cdfs, ecdf_masses), w)
  expect_equal(mix_quantile(mix, probs), brute_quantile(cdfs, w, probs))
  # the returned step function is the mixture
  expect_equal(mix_ecdf(mix)(mix$x),
               Reduce(`+`, Map(function(f, ww) ww * f(mix$x), cdfs, w)))
  # cells with zero weight are skipped
  expect_equal(mix_quantile(mix_cdf(lapply(cdfs, ecdf_masses), c(0.6, 0.4, 0)),
                            probs),
               brute_quantile(cdfs[1:2], c(0.6, 0.4), probs))
  # an exact quantile at a jump (F = tau exactly) is that point, as type 1
  two <- list(BMisc::weighted_ecdf(c(1, 2, 3, 4)))
  expect_equal(mix_quantile(mix_cdf(lapply(two, ecdf_masses), 1),
                            c(0.25, 0.5, 0.75)), c(1, 2, 3))
  expect_error(ecdf_masses(function(y) y), "step-function CDFs")
})

# local QTT: F1 from the treated, F0 from the comparison units, in the post
# period (weighted); not a sensible estimator, but enough to test aggregation
qtt_cell <- function(gt_data, ...) {
  post <- gt_data[gt_data$name == "post", ]
  F1 <- BMisc::weighted_ecdf(post$Y[post$D == 1], # nolint: object_name_linter
                             weights = post$.w[post$D == 1])
  F0 <- BMisc::weighted_ecdf(post$Y[post$D == 0], # nolint: object_name_linter
                             weights = post$.w[post$D == 0])
  attgt_noif(attgt = 0, extra_gt_returns = list(F0 = F0, F1 = F1))
}

run_qtt <- function(data, weightsname = NULL) {
  suppressMessages(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = data, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = qtt_cell,
    aggte_fun = qtt_pte_aggregations, gt_type = "qtt",
    boot_type = "empirical", weightsname = weightsname,
    probs = seq(0.1, 0.9, 0.1), biters = 0
  ))
}

test_that("integer weights give the same QTTs as replicated rows", {
  skip_if_not_installed("did")
  d <- did::mpdta
  set.seed(2)
  reps <- sample(1:3, 500, replace = TRUE)
  d$w <- reps[match(d$countyreal, unique(d$countyreal))]
  weighted <- run_qtt(d, "w")
  # replicate each county w times, as new units
  rows <- rep(seq_len(nrow(d)), d$w)
  dr <- d[rows, ]
  dr$countyreal <- dr$countyreal * 10 + stats::ave(rows, rows, FUN = seq_along)
  replicated <- run_qtt(dr)
  expect_equal(weighted$overall$qtt, replicated$overall$qtt)
  expect_equal(weighted$dynamic$qtt, replicated$dynamic$qtt)
  expect_equal(weighted$group$qtt, replicated$group$qtt)
})

test_that("the overall QTT is the quantile of the exact mixture", {
  skip_if_not_installed("did")
  ptep <- setup_pte(yname = "lemp", gname = "first.treat", tname = "year",
                    idname = "countyreal", data = did::mpdta,
                    gt_type = "qtt", probs = seq(0.1, 0.9, 0.1))
  gt <- compute.pte(ptep, subset_fun = two_by_two_subset,
                    attgt_fun = qtt_cell)
  agg <- qtt_pte_aggregations(gt$attgt.list, ptep, gt$extra_gt_returns)
  w <- attgt_pte_aggregations(gt$attgt.list, ptep)$overall_weights
  cells <- lapply(gt$extra_gt_returns, `[[`, "extra_gt_returns")
  probs <- seq(0.1, 0.9, 0.1)
  expected <- brute_quantile(lapply(cells, `[[`, "F1"), w, probs) -
    brute_quantile(lapply(cells, `[[`, "F0"), w, probs)
  expect_equal(agg$overall_results$qtt, expected)
})
