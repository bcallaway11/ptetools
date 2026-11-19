# =============================================================================
# Title: Tests for exact QTT aggregation
# Description: qtt_pte_aggregations() mixes the cells' CDFs exactly, from
#   every support point (BMisc::combine_ecdfs()), rather than on a
#   1,000-point grid of the pooled outcome (#25). Checked against a brute-force mixture, and integer weights
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
  expect_equal(mix_quantile(cdfs, w, probs), brute_quantile(cdfs, w, probs))
  # cells with zero weight are skipped
  expect_equal(mix_quantile(cdfs, c(0.6, 0.4, 0), probs),
               brute_quantile(cdfs[1:2], c(0.6, 0.4), probs))
  # an exact quantile at a jump (F = tau exactly) is that point, as type 1
  two <- list(BMisc::weighted_ecdf(c(1, 2, 3, 4)))
  expect_equal(mix_quantile(two, 1, c(0.25, 0.5, 0.75)), c(1, 2, 3))
  expect_error(mix_quantile(list(function(y) y), 1, 0.5), "not ecdf objects")
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


# --- Quantile of the treatment effect (QOTT) ---------------------------------
# The distribution of the treatment effect was mixed on 1,000 points of
# [-max(y), max(y)]: rounded, blind to effects below -max(y), and backwards
# when every outcome is negative. It is now exact, as for the QTT.

# local distribution of the treatment effect: treated units' change in Y
# minus the comparison units' mean change (enough to test aggregation)
qott_cell <- function(gt_data, ...) {
  pp <- gt_pre_post(gt_data)
  dy <- pp$post$Y - pp$pre$Y
  te <- dy[pp$pre$D == 1] - mean(dy[pp$pre$D == 0])
  attgt_noif(attgt = mean(te),
             extra_gt_returns = list(Fte = BMisc::weighted_ecdf(te)))
}

qott_by_hand <- function(data, ret_quantile) {
  ptep <- setup_pte(yname = "y", gname = "grp", tname = "period",
                    idname = "id", data = data, gt_type = "qott",
                    ret_quantile = ret_quantile)
  gt <- compute.pte(ptep, subset_fun = two_by_two_subset,
                    attgt_fun = qott_cell)
  list(
    agg = qott_pte_aggregations(gt$attgt.list, ptep, gt$extra_gt_returns),
    w = attgt_pte_aggregations(gt$attgt.list, ptep),
    cells = lapply(gt$extra_gt_returns,
                   function(e) e$extra_gt_returns$Fte)
  )
}

test_that("QOTT aggregation is exact, also with negative outcomes", {
  set.seed(3)
  n <- 400
  grp <- sample(c(0, 3, 4), n, replace = TRUE)
  d <- expand.grid(id = seq_len(n), period = 1:4)
  d$grp <- grp[d$id]
  # all outcomes negative, effects far below -max(y)
  d$y <- -50 + rnorm(n)[d$id] + rnorm(nrow(d)) -
    20 * (d$grp > 0 & d$period >= d$grp)
  expect_true(all(d$y < 0))
  for (tau in c(0.1, 0.5, 0.9)) {
    r <- qott_by_hand(d, tau)
    expect_equal(unname(r$agg$overall_results),
                 brute_quantile(r$cells, r$w$overall_weights, tau))
    expect_equal(names(r$agg$overall_results), paste0(100 * tau, "%"))
    for (j in seq_along(r$w$dyn_weights)) {
      expect_equal(unname(r$agg$dyn_results$att.e[j]),
                   brute_quantile(r$cells, r$w$dyn_weights[[j]]$weights, tau))
    }
    # each cell's QOTT is its own exact quantile
    expect_equal(r$agg$attgt_results$att,
                 vapply(r$cells, function(f) brute_quantile(list(f), 1, tau),
                        numeric(1)))
  }
  # effects are about -20, far below -max(y) (about 47)
  expect_lt(unname(qott_by_hand(d, 0.5)$agg$overall_results), -15)
})


# --- Aggregation weights ----------------------------------------------------
# local ATT and QTT: post-period treated vs comparison units (randomized
# design below, so this is enough to test the inference)
qtt_att_cell <- function(gt_data, ...) {
  post <- gt_data[gt_data$name == "post", ]
  y1 <- post$Y[post$D == 1]
  y0 <- post$Y[post$D == 0]
  attgt_noif(attgt = mean(y1) - mean(y0),
             extra_gt_returns = list(F0 = BMisc::weighted_ecdf(y0),
                                     F1 = BMisc::weighted_ecdf(y1)))
}

qtt_att_data <- function(effect) {
  set.seed(5)
  n <- 600
  grp <- sample(c(0, 2, 3), n, replace = TRUE)
  d <- expand.grid(id = seq_len(n), period = 1:3)
  d$grp <- grp[d$id]
  treated <- d$grp > 0 & d$period >= d$grp
  d$y <- rnorm(nrow(d)) + ifelse(treated, effect(sum(treated)), 0)
  d
}

# The weight vectors were in the row order of an internal table that
# merge() sorts by group, not in the order of the cells, so the QTT and
# QOTT aggregations mixed the wrong cells' distributions (found with #27).
test_that("aggregation weights follow the order of the cells", {
  d <- qtt_att_data(function(n) 1)
  ptep <- setup_pte(yname = "y", gname = "grp", tname = "period",
                    idname = "id", data = d, gt_type = "qtt")
  check <- function(attgt_fun) {
    gt <- compute.pte(ptep, two_by_two_subset, attgt_fun)
    cells <- do.call(rbind, lapply(gt$attgt.list, as.data.frame))
    agg <- attgt_pte_aggregations(gt$attgt.list, ptep)
    att <- ifelse(is.na(cells$att), 0, cells$att)
    expect_equal(length(agg$overall_weights), nrow(cells))
    expect_equal(sum(agg$overall_weights * att), agg$overall_results)
    for (dw in agg$dyn_weights) {
      expect_equal(sum(dw$weights * att),
                   agg$dyn_results$att.e[agg$dyn_results$e == dw$e])
    }
    # no weight on pre-treatment or dropped cells
    expect_true(all(agg$overall_weights[cells$time.period < cells$group |
                                          is.na(cells$att)] == 0))
  }
  check(qtt_att_cell)
  # a cell dropped as NA (e.g. an overlap failure) shifted later weights
  check(function(gt_data, ...) {
    res <- qtt_att_cell(gt_data)
    post <- gt_data[gt_data$name == "post", ]
    if (unique(post$period) == 2) res$attgt <- NA
    res
  })
})
