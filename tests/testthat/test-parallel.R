# =============================================================================
# Title: Tests for the parallel empirical bootstrap
# Description: A numeric `cl` runs the empirical bootstrap in parallel by
#   forking on Unix/macOS; on Windows, which cannot fork, ptetools creates a
#   PSOCK cluster (#11). Cluster workers load the *installed* ptetools, so
#   the cluster tests only run where that is the code under test (R CMD
#   check), not under devtools::load_all().
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

test_that("boot_cluster() only creates a cluster for numeric cl on Windows", {
  expect_identical(boot_cluster(1, os = "windows"), 1)
  expect_identical(boot_cluster(4, os = "unix"), 4)
  expect_identical(boot_cluster(NULL, os = "windows"), NULL)
})

# local ATT without an influence function, so pte() uses the empirical
# bootstrap; written as a user would, calling attgt_noif() without ptetools::
noif_attgt <- function(gt_data, ...) {
  post <- gt_data$name == "post"
  attgt_noif(attgt = mean(gt_data$Y[gt_data$D == 1 & post]) -
               mean(gt_data$Y[gt_data$D == 0 & post]))
}

run_emp_boot <- function(cl, seed) {
  set.seed(seed)
  res <- suppressWarnings(suppressMessages(pte(
    yname = "lemp", gname = "first.treat", tname = "year",
    idname = "countyreal", data = did::mpdta, setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset, attgt_fun = noif_attgt, biters = 20,
    cl = cl
  )))
  c(att = res$overall_results$att, se = res$overall_results$se)
}

test_that("the empirical bootstrap runs on a PSOCK cluster", {
  skip_on_cran()
  skip_if_not_installed("did")
  skip_if(exists(".__DEVTOOLS__", envir = asNamespace("ptetools")),
          "cluster workers would load the installed, not the loaded, ptetools")

  # the cluster boot_cluster() builds on Windows
  cl <- boot_cluster(2, os = "windows")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  expect_s3_class(cl, "cluster")

  serial <- run_emp_boot(1, seed = 1)
  parallel::clusterSetRNGStream(cl, 1)
  par1 <- run_emp_boot(cl, seed = 1)
  parallel::clusterSetRNGStream(cl, 1)
  par2 <- run_emp_boot(cl, seed = 1)

  # same point estimate; bootstrap SEs reproducible with the same streams
  expect_equal(par1[["att"]], serial[["att"]])
  expect_true(is.finite(par1[["se"]]) && par1[["se"]] > 0)
  expect_identical(par1, par2)
})
