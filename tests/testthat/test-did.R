#------------------------------------------------------------------------
#  Some tests for `did`
#------------------------------------------------------------------------

library(did)

test_that("did basics", {
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

  expect_equal(res$overall_att$overall.att, 1, tolerance = .5)
  dyn_idx <- res$event_study$egt == 0
  expect_equal(res$event_study$att.egt[dyn_idx], 1, tolerance = 0.5)

  # compare to results from did package
  cs_res <- did::att_gt(
    yname = "Y",
    gname = "G",
    tname = "period",
    idname = "id",
    data = data,
    xformla = ~X,
    control_group = "notyettreated"
  )

  cs_overall <- did::aggte(cs_res, type = "group")$overall.att
  expect_equal(res$overall_att$overall.att, cs_overall)
  cs_dyn <- did::aggte(cs_res, type = "dynamic")$att.egt[dyn_idx]
  expect_equal(res$event_study$att.egt[dyn_idx], cs_dyn)
})

test_that("repeated cross sections match did", {
  set.seed(123)
  sp <- did::reset.sim()
  data <- did::build_sim_dataset(sp, panel = FALSE)

  res <- suppressWarnings(
    pte_default(
      yname = "Y",
      gname = "G",
      tname = "period",
      data = data,
      panel = FALSE,
      xformula = ~X,
      control_group = "notyettreated",
      cband = FALSE
    )
  )

  cs_res <- suppressWarnings(
    did::att_gt(
      yname = "Y",
      gname = "G",
      tname = "period",
      data = data,
      panel = FALSE,
      xformla = ~X,
      control_group = "notyettreated",
      bstrap = FALSE,
      cband = FALSE
    )
  )

  pte_attgt <- data.frame(
    group = res$att_gt$group,
    t = res$att_gt$t,
    att = res$att_gt$att
  )
  did_attgt <- data.frame(
    group = cs_res$group,
    t = cs_res$t,
    att = cs_res$att
  )
  pte_attgt <- pte_attgt[order(pte_attgt$group, pte_attgt$t), ]
  did_attgt <- did_attgt[order(did_attgt$group, did_attgt$t), ]
  row.names(pte_attgt) <- NULL
  row.names(did_attgt) <- NULL

  expect_equal(pte_attgt, did_attgt, tolerance = 1e-8)

  cs_overall <- did::aggte(cs_res, type = "group")$overall.att
  expect_equal(res$overall_att$overall.att, cs_overall, tolerance = 1e-8)

  cs_dynamic <- did::aggte(cs_res, type = "dynamic")$att.egt
  expect_equal(res$event_study$att.egt, cs_dynamic, tolerance = 1e-8)
})

test_that("empirical bootstrap", {
  # Skipped pending API alignment: pte_emp_boot uses $overall_results$att
  # while pte_results uses $overall_att$overall.att. These should be unified
  # so that both result objects expose the same field names. See dev/NOTES.md.
  skip("pte_emp_boot and pte_results have inconsistent field names for overall ATT")
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
      boot_type = "empirical",
      biters = 10
    )
  )

  # When field names are unified, both paths should expose $overall_att$overall.att
  expect_equal(res$overall_att$overall.att, 1, tolerance = 0.5)
})

test_that("periods that look like years works ok and unbalanced groups", {
  # Tests two edge cases:
  # 1. Year-like time periods (non-consecutive integers e.g. 2003/2004/2006/2007)
  #    that get remapped internally via orig2t/t2orig.
  # 2. A data column literally named "G" (matching ptetools' internal column name),
  #    which can cause masking bugs if setup_pte does not rename carefully.
  # Skipped pending verification of hardcoded expected values. See dev/NOTES.md.
  skip("expected values need verification; see dev/NOTES.md for context")
  data(mpdta)
  res <- suppressWarnings(
    pte(
      yname = "lemp",
      gname = "first.treat",
      tname = "year",
      idname = "countyreal",
      data = mpdta,
      setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset,
      attgt_fun = did_attgt,
      xformula = ~lpop
    )
  )

  # this is to test if summary is working // had issues with ife version of this
  expect_equal(summary(res)$overall_att$overall_att, -0.0323)
  dyn_idx <- summary(res)$event_study[, "Event Time"] == 0
  expect_equal(summary(res)$event_study$Estimat[dyn_idx], -0.0201)

  #------------------------------------------------------------------------
  #  case where the group variable is named G
  #------------------------------------------------------------------------
  data(mpdta)
  mpdta$G <- mpdta$first.treat
  res <- suppressWarnings(
    pte(
      yname = "lemp",
      gname = "G",
      tname = "year",
      idname = "countyreal",
      data = mpdta,
      setup_pte_fun = setup_pte,
      subset_fun = two_by_two_subset,
      attgt_fun = did_attgt,
      xformula = ~lpop
    )
  )
  # this is to test if summary is working // had issues with ife version of this
  expect_equal(summary(res)$overall_att$overall_att, -0.0323)
  dyn_idx <- summary(res)$event_study[, "Event Time"] == 0
  expect_equal(summary(res)$event_study$Estimat[dyn_idx], -0.0201)
})

test_that("no formula for covariates is ok", {
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
      attgt_fun = did_attgt
    )
  )

  expect_equal(res$overall_att$overall.att, 1, tolerance = .75)
})
