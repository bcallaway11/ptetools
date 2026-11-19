# =============================================================================
# Title: Tests for clustered standard errors
# Description: clustervars (as in did::att_gt()) clusters analytical standard
#   errors, the multiplier bootstrap and the empirical bootstrap at a level
#   above the unit (#6).
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

# units nested in clusters, with cluster-by-period shocks, so clustering
# matters
cluster_data <- function() {
  set.seed(10)
  n_cl <- 40
  per_cl <- 10
  n <- n_cl * per_cl
  n_periods <- 4
  cl <- rep(seq_len(n_cl), each = per_cl)
  g_cl <- sample(c(0, 3, 4), n_cl, replace = TRUE)
  d <- expand.grid(id = seq_len(n), period = seq_len(n_periods))
  d$cl <- cl[d$id]
  d$grp <- g_cl[d$cl]
  shock <- matrix(rnorm(n_cl * n_periods), n_cl)
  d$y <- rnorm(n)[d$id] + shock[cbind(d$cl, d$period)] +
    (d$grp > 0 & d$period >= d$grp) + rnorm(nrow(d))
  d
}

run_cluster <- function(..., seed = 1) {
  set.seed(seed)
  suppressWarnings(pte_default(
    yname = "y", gname = "grp", tname = "period", idname = "id",
    data = cluster_data(), d_outcome = TRUE, est_method = "reg",
    cband = FALSE, ...
  ))
}

test_that("clustering on idname is the same as not clustering", {
  a <- run_cluster(biters = 200)
  b <- run_cluster(biters = 200, clustervars = "id")
  expect_equal(b$att_gt$se, a$att_gt$se)
  expect_equal(b$overall_att$overall.se, a$overall_att$overall.se)
  a0 <- run_cluster(bstrap = FALSE)
  b0 <- run_cluster(bstrap = FALSE, clustervars = "id")
  expect_equal(b0$event_study$se.egt, a0$event_study$se.egt)
})

test_that("analytical clustered SEs use within-cluster sums", {
  res <- run_cluster(bstrap = FALSE, clustervars = "cl")
  inf <- as.numeric(res$overall_att$inf.function[[2]])
  d <- cluster_data()
  unit_cluster <- d$cl[match(sort(unique(d$id)), d$id)]
  expected <- sqrt(sum(rowsum(inf, unit_cluster)^2)) / length(inf)
  expect_equal(res$overall_att$overall.se, expected)
  # point estimates do not depend on clustering
  expect_equal(res$att_gt$att, run_cluster(bstrap = FALSE)$att_gt$att)
})

test_that("bootstrap and analytical clustered SEs agree", {
  analytical <- run_cluster(bstrap = FALSE, clustervars = "cl")
  boot <- run_cluster(biters = 2000, clustervars = "cl")
  unclustered <- run_cluster(bstrap = FALSE)
  expect_equal(boot$overall_att$overall.se,
               analytical$overall_att$overall.se, tolerance = 0.1)
  # with cluster-level shocks, clustering raises the standard errors
  expect_gt(analytical$overall_att$overall.se,
            1.5 * unclustered$overall_att$overall.se)
})

test_that("the empirical bootstrap resamples clusters", {
  emp <- function(...) {
    set.seed(1)
    suppressMessages(suppressWarnings(pte(
      yname = "y", gname = "grp", tname = "period", idname = "id",
      data = cluster_data(), setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset, attgt_fun = pte_attgt, xformula = ~1,
      d_outcome = TRUE, est_method = "reg", boot_type = "empirical",
      biters = 100, ...
    )))
  }
  clustered <- emp(clustervars = "cl")
  analytical <- run_cluster(bstrap = FALSE, clustervars = "cl")
  expect_equal(clustered$overall_results$se,
               analytical$overall_att$overall.se, tolerance = 0.25)
  expect_gt(clustered$overall_results$se, 1.5 * emp()$overall_results$se)
})

test_that("invalid cluster variables give clear errors", {
  d <- cluster_data()
  d$cl2 <- d$cl
  d$varying <- d$period
  run <- function(cv) {
    pte_default(yname = "y", gname = "grp", tname = "period", idname = "id",
                data = d, clustervars = cv, biters = 0)
  }
  expect_error(run(c("cl", "cl2")), "At most one cluster variable")
  expect_error(run("varying"), "constant over time within each unit")
  expect_error(run("nope"), "not a column of data")
})
