# =============================================================================
# Title: Tests for sampling weights
# Description: Aggregated standard errors must not depend on the scale of the
#   sampling weights (issue #8) and should match did::att_gt(). Weights
#   audit (#21): weighted estimates and SEs match did::att_gt() (panel,
#   repeated cross sections, clustered), did_attgt(), covid_attgt() and
#   setup_pte_basic() use weights, and empirical-bootstrap draws are
#   weighted.
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
# never whole numbers, so every doubly robust call warned "non-integer
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
      xformula = ~lpop, est_method = "aipw", weightsname = wname, biters = 0
    ))
  }
})

# The empirical-bootstrap aggregation weighted cohorts by row counts, not by
# sampling weights (#23). With weights that differ across cohorts, its
# overall ATT and event study must match pte_aggte() (the multiplier path),
# and integer weights must match duplicating units.
test_that("empirical-bootstrap aggregation uses sampling weights", {
  skip_if_not_installed("did")
  data <- did::mpdta
  set.seed(1)
  data$w <- rep(sample(1:5, 500, replace = TRUE), each = 5)
  data$w[data$first.treat == 2007] <- data$w[data$first.treat == 2007] * 4
  run <- function(d, boot_type, idname = "countyreal", weightsname = "w") {
    suppressMessages(suppressWarnings(pte(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = idname, data = d, setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset, attgt_fun = pte_attgt,
      xformula = ~1, d_outcome = TRUE, boot_type = boot_type, biters = 5,
      weightsname = weightsname
    )))
  }
  mult <- run(data, "multiplier")
  emp <- run(data, "empirical")
  expect_equal(emp$overall_results$att, mult$overall_att$overall.att)
  expect_equal(emp$dyn_results$att.e[match(mult$event_study$egt,
                                           emp$dyn_results$e)],
               mult$event_study$att.egt)

  # integer weights = each unit duplicated w times (with new ids)
  replicated <- data[rep(seq_len(nrow(data)), data$w), ]
  replicated$copy <- stats::ave(replicated$year, replicated$countyreal,
                                replicated$year, FUN = seq_along)
  replicated$newid <- as.integer(factor(paste(replicated$countyreal,
                                              replicated$copy)))
  rep_res <- run(replicated, "empirical", idname = "newid",
                 weightsname = NULL)
  expect_equal(rep_res$overall_results$att, emp$overall_results$att)
})

test_that("invalid sampling weights give clear errors", {
  skip_if_not_installed("did")
  run <- function(w) {
    d <- did::mpdta
    d$w <- w
    pte_default(yname = "lemp", gname = "first.treat", tname = "year",
                idname = "countyreal", data = d, d_outcome = TRUE,
                weightsname = "w", biters = 0)
  }
  good <- rep(seq(1, 2, length.out = 500), each = 5)
  bad <- good
  bad[3] <- NA
  expect_error(run(bad), "non-missing and finite")
  bad <- good
  bad[3] <- -1
  expect_error(run(bad), "non-negative")
  expect_error(run(good * rep(1:5, 500)), "constant over time within each unit")
})


# --- Weights audit (#21) ----------------------------------------------------

weighted_mpdta <- function() {
  d <- did::mpdta
  set.seed(1)
  d$w <- rep(stats::rexp(500), each = 5)
  d$state <- rep(sample(1:40, 500, replace = TRUE), each = 5)
  d
}

did_weighted <- function(d, ...) {
  suppressMessages(suppressWarnings(did::att_gt(
    yname = "lemp", tname = "year", idname = "countyreal",
    gname = "first.treat", data = d, weightsname = "w", bstrap = FALSE,
    cband = FALSE, ...
  )))
}

# max absolute differences in ATT(g,t) and their SEs, matched by (g,t)
attgt_diff <- function(a, b) {
  m <- match(paste(a$att_gt$group, a$att_gt$t), paste(b$group, b$t))
  # (SEs at a universal reference period are NA in both packages)
  max(abs(c(a$att_gt$att - b$att[m], a$att_gt$se - b$se[m])), na.rm = TRUE)
}

test_that("weighted estimates and SEs match did::att_gt()", {
  skip_if_not_installed("did")
  d <- weighted_mpdta()
  grid <- expand.grid(est = c("aipw", "reg"), bp = c("varying", "universal"),
                      cg = c("notyettreated", "nevertreated"),
                      stringsAsFactors = FALSE)
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    a <- suppressWarnings(pte_default(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = d, xformula = ~lpop, d_outcome = TRUE,
      est_method = g$est, base_period = g$bp, control_group = g$cg,
      weightsname = "w", bstrap = FALSE, cband = FALSE
    ))
    b <- did_weighted(d, xformla = ~lpop, control_group = g$cg,
                      est_method = if (g$est == "aipw") "dr" else "reg",
                      base_period = g$bp)
    label <- paste(g, collapse = "/")
    expect_lt(attgt_diff(a, b), 1e-10, label = label)
    dyn <- did::aggte(b, type = "dynamic", bstrap = FALSE)
    grp <- did::aggte(b, type = "group", bstrap = FALSE)
    em <- match(a$event_study$egt, dyn$egt)
    expect_lt(max(abs(a$event_study$se.egt - dyn$se.egt[em]), na.rm = TRUE),
              1e-10, label = label)
    expect_lt(abs(a$overall_att$overall.se - grp$overall.se), 1e-10,
              label = label)
  }
  # repeated cross sections: ATT(g,t) and their SEs
  a <- suppressWarnings(pte_default(
    yname = "lemp", gname = "first.treat", tname = "year", data = d,
    panel = FALSE, xformula = ~lpop, weightsname = "w", bstrap = FALSE,
    cband = FALSE
  ))
  b <- did_weighted(d, xformla = ~lpop, panel = FALSE,
                    control_group = "notyettreated")
  expect_lt(attgt_diff(a, b), 1e-10)
  # clustered and weighted
  a <- suppressWarnings(pte_default(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, xformula = ~lpop, d_outcome = TRUE,
    weightsname = "w", clustervars = "state", bstrap = FALSE, cband = FALSE
  ))
  b <- did_weighted(d, xformla = ~lpop, control_group = "notyettreated",
                    clustervars = "state")
  expect_lt(attgt_diff(a, b), 1e-10)
})

# did_attgt() did not pass the weights to DRDID, so it ignored them
test_that("did_attgt() uses sampling weights", {
  skip_if_not_installed("did")
  d <- weighted_mpdta()
  a <- suppressWarnings(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = did_attgt,
    xformula = ~lpop, weightsname = "w", bstrap = FALSE, cband = FALSE
  ))
  b <- did_weighted(d, xformla = ~lpop, control_group = "notyettreated")
  expect_lt(attgt_diff(a, b), 1e-10)
})

# covid_attgt() did not pass the weights to DRDID either. With first
# differences it is the doubly robust DID estimator.
test_that("covid_attgt() uses sampling weights", {
  skip_if_not_installed("did")
  d <- weighted_mpdta()
  a <- suppressWarnings(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = covid_attgt,
    xformla = ~lpop, d_outcome = TRUE, weightsname = "w", bstrap = FALSE,
    cband = FALSE
  ))
  b <- did_weighted(d, xformla = ~lpop, control_group = "notyettreated")
  expect_lt(attgt_diff(a, b), 1e-10)
})

# setup_pte_basic() had no weightsname and set every weight to 1
test_that("setup_pte_basic() uses sampling weights", {
  skip_if_not_installed("did")
  d <- weighted_mpdta()
  run <- function(setup) {
    suppressWarnings(pte(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = d, setup_pte_fun = setup,
      subset_fun = two_by_two_subset, attgt_fun = pte_attgt,
      xformula = ~1, d_outcome = TRUE, weightsname = "w", bstrap = FALSE,
      cband = FALSE
    ))
  }
  basic <- run(setup_pte_basic)
  full <- run(setup_pte)
  expect_equal(basic$att_gt$att, full$att_gt$att)
  expect_equal(basic$att_gt$se, full$att_gt$se)
})

# The empirical bootstrap rebuilt each draw without weightsname (a named
# argument of pte(), so not in `...`): every draw was unweighted, and
# weighted SEs equaled the unweighted ones.
test_that("empirical-bootstrap draws use sampling weights", {
  skip_if_not_installed("did")
  skip_on_cran()
  d <- weighted_mpdta()
  analytical <- suppressWarnings(pte_default(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, d_outcome = TRUE, est_method = "reg",
    weightsname = "w", bstrap = FALSE, cband = FALSE
  ))
  set.seed(2)
  emp <- suppressMessages(suppressWarnings(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = pte_attgt, xformula = ~1,
    d_outcome = TRUE, est_method = "reg", weightsname = "w",
    boot_type = "empirical", biters = 300
  )))
  # unweighted draws gave 0.0148 here, against 0.0212 analytically
  em3 <- analytical$event_study$egt == -3
  expect_equal(emp$dyn_results$se[emp$dyn_results$e == -3],
               analytical$event_study$se.egt[em3], tolerance = 0.15)
  expect_equal(emp$overall_results$se, analytical$overall_att$overall.se,
               tolerance = 0.15)

  # QTT runs: the ATT from the same draws (#27) against the analytical SE
  # of the same estimator (difference in weighted post-period means)
  qcell <- function(gt_data, ...) {
    post <- gt_data[gt_data$name == "post", ]
    i1 <- post$D == 1
    i0 <- post$D == 0
    wm <- function(i) stats::weighted.mean(post$Y[i], post$.w[i])
    attgt_noif(attgt = wm(i1) - wm(i0), extra_gt_returns = list(
      F0 = BMisc::weighted_ecdf(post$Y[i0], weights = post$.w[i0]),
      F1 = BMisc::weighted_ecdf(post$Y[i1], weights = post$.w[i1])
    ))
  }
  set.seed(3)
  qtt <- suppressMessages(suppressWarnings(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = qcell,
    aggte_fun = qtt_pte_aggregations, gt_type = "qtt",
    boot_type = "empirical", probs = 0.5, weightsname = "w", biters = 300
  )))
  levels <- suppressWarnings(pte_default(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = d, d_outcome = FALSE, est_method = "reg",
    weightsname = "w", bstrap = FALSE, cband = FALSE
  ))
  expect_equal(qtt$att$overall$att, levels$overall_att$overall.att)
  expect_equal(qtt$att$overall$se, levels$overall_att$overall.se,
               tolerance = 0.15)
})

# The QOTT bootstrap draws were rebuilt without ret_quantile, so they always
# used the default (the median), whatever quantile was estimated
test_that("QOTT bootstrap draws use ret_quantile", {
  skip_if_not_installed("did")
  te_cell <- function(gt_data, ...) {
    pp <- gt_pre_post(gt_data)
    dy <- pp$post$Y - pp$pre$Y
    te <- dy[pp$pre$D == 1] - mean(dy[pp$pre$D == 0])
    attgt_noif(attgt = mean(te),
               extra_gt_returns = list(Fte = BMisc::weighted_ecdf(te)))
  }
  run <- function(tau) {
    set.seed(4)
    suppressMessages(suppressWarnings(pte(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = did::mpdta, setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset, attgt_fun = te_cell,
      aggte_fun = qott_pte_aggregations, gt_type = "qott",
      boot_type = "empirical", ret_quantile = tau, biters = 20
    )))$overall_results$se
  }
  # the same draws: SEs differ only if the draws estimate the right quantile
  expect_false(isTRUE(all.equal(run(0.1), run(0.5))))
})
