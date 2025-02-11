#------------------------------------------------------------------------
#  inference tests for did
#------------------------------------------------------------------------

library(did)
library(pbapply)

test_that("tests for inference", {
  skip_on_cran()
  set.seed(123)
  cl <- 1
  mc_sims <- 100
  rejs <- pblapply(1:mc_sims, function(mc) {
    sp <- did::reset.sim()
    data <- did::build_sim_dataset(sp)

    res <- suppressWarnings(
      pte(
        yname = "Y",
        gname = "G",
        tname = "period",
        idname = "id",
        data = data,
        setup_pte_fun = setup_pte,
        subset_fun = two_by_two_subset,
        attgt_fun = did_attgt,
        xformula = ~X
      )
    )
    # truth is that att = 1
    tstat <- (res$overall_att$overall.att - 1) / res$overall_att$overall.se
    rej <- 1 * (abs(tstat) > qnorm(.975))
    rej
  }, cl = cl)

  rej_frac <- mean(unlist(rejs))

  expect_true(abs(rej_frac - 0.06) <= 0.05) # make test fail if reject 0 times
})
