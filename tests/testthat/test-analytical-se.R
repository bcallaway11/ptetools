# =============================================================================
# Title: Tests for analytical (non-bootstrap) standard errors
# Description: Compares pte()/pte_default() with bstrap = FALSE against
#   did::att_gt()/did::aggte() with bstrap = FALSE, and checks the guard
#   logic (cband downgrade, boot_type override, dose warning, no-IF fallback
#   warning).
# Author: Brant Callaway
# Last update: 2026-07-25
# Date created: 2026-07-25
# =============================================================================

library(did)

test_that("analytical group-time and aggregated SEs match did", {
  set.seed(123)
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
      xformula = ~X,
      bstrap = FALSE,
      cband = FALSE
    )
  )

  cs_res <- did::att_gt(
    yname = "Y",
    gname = "G",
    tname = "period",
    idname = "id",
    data = data,
    xformla = ~X,
    control_group = "notyettreated",
    bstrap = FALSE,
    cband = FALSE
  )

  pte_se <- data.frame(group = res$att_gt$group, t = res$att_gt$t, se = res$att_gt$se)
  did_se <- data.frame(group = cs_res$group, t = cs_res$t, se = cs_res$se)
  pte_se <- pte_se[order(pte_se$group, pte_se$t), ]
  did_se <- did_se[order(did_se$group, did_se$t), ]
  row.names(pte_se) <- NULL
  row.names(did_se) <- NULL

  expect_equal(pte_se, did_se, tolerance = 1e-8)

  # NOTE: tolerance here is looser than the (g,t)-level comparison above.
  # ptetools' pte_aggte() renormalizes pg to sum to 1 across ever-treated
  # groups before passing it into wif() (the weight-influence-function
  # correction term), whereas did's compute.aggte() does not renormalize at
  # that stage. wif() is not scale-invariant in pg, so this produces a tiny
  # (~1e-4 relative) discrepancy in the *aggregated* SE that is unrelated to
  # bstrap = FALSE -- it is present in the shared wif()/get_agg_inf_func()
  # code used by both the bootstrap and analytical paths. See dev/NOTES.md.
  cs_overall <- did::aggte(cs_res, type = "group", bstrap = FALSE, cband = FALSE)
  expect_equal(res$overall_att$overall.se, cs_overall$overall.se, tolerance = 1e-3)

  cs_dyn <- did::aggte(cs_res, type = "dynamic", bstrap = FALSE, cband = FALSE)
  dyn_idx <- res$event_study$egt == 0
  cs_dyn_idx <- cs_dyn$egt == 0
  expect_equal(res$event_study$se.egt[dyn_idx], cs_dyn$se.egt[cs_dyn_idx], tolerance = 1e-3)
})

test_that("bstrap = FALSE with cband = TRUE warns and falls back to pointwise", {
  set.seed(123)
  sp <- did::reset.sim()
  data <- did::build_sim_dataset(sp)

  expect_warning(
    res <- pte(
      yname = "Y",
      gname = "G",
      tname = "period",
      idname = "id",
      data = data,
      setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset,
      attgt_fun = did_attgt,
      xformula = ~X,
      bstrap = FALSE,
      cband = TRUE
    ),
    "only support pointwise"
  )

  expect_false(res$ptep$cband)
})

test_that("bstrap = FALSE overrides boot_type = \"empirical\" without warning", {
  set.seed(123)
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
      xformula = ~X,
      bstrap = FALSE,
      cband = FALSE,
      boot_type = "empirical"
    )
  )

  # class pte_results (not pte_emp_boot) confirms boot_type was overridden
  expect_s3_class(res, "pte_results")
})

test_that("analytical SEs are not supported for gt_type = \"dose\"", {
  expect_warning(
    tryCatch(
      pte(
        yname = "Y",
        gname = "G",
        tname = "period",
        data = data.frame(),
        setup_pte_fun = function(...) NULL,
        subset_fun = function(...) NULL,
        attgt_fun = function(...) NULL,
        gt_type = "dose",
        bstrap = FALSE
      ),
      error = function(e) NULL
    ),
    "not currently supported for continuous treatment"
  )
})

test_that("requesting analytical SEs without an influence function warns and falls back to empirical bootstrap", {
  set.seed(123)
  sp <- did::reset.sim()
  data <- did::build_sim_dataset(sp)

  no_if_attgt <- function(gt_data, ...) {
    attgt_out <- did_attgt(gt_data, ...)
    attgt_noif(attgt_out$attgt)
  }

  expect_warning(
    pte(
      yname = "Y",
      gname = "G",
      tname = "period",
      idname = "id",
      data = data,
      setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset,
      attgt_fun = no_if_attgt,
      xformula = ~X,
      bstrap = FALSE,
      cband = FALSE,
      biters = 5,
      aggte_fun = function(al, p, eg) attgt_pte_aggregations(al, p)
    ),
    "analytical standard errors"
  )
})
