# =============================================================================
# Title: Tests for column-name collisions (#13)
# Description: User columns whose names match ptetools' internal names
#   (G, id, period, Y, ...) must give the same results as any other names,
#   including when groups and periods are coded as calendar years. Also tests
#   the gt_data_frame() contract that subset functions rely on.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

# --- Simulated data: groups and periods on the calendar-year scale -------
collision_data <- function() {
  set.seed(1)
  n <- 400
  grp <- sample(c(0, 2003, 2004), n, replace = TRUE)
  d <- data.frame(
    unit = rep(seq_len(n), each = 5), year = rep(2001:2005, n),
    grp = rep(grp, each = 5), x = rep(rnorm(n), each = 5)
  )
  d$y <- d$x + rnorm(nrow(d)) + 1 * (d$grp > 0 & d$year >= d$grp)
  d
}

# Estimates with the user's columns given the names in `nm` (a named vector:
# gname, tname, idname, yname); the data keep the original columns too, so a
# collision cannot be avoided by the other copy being absent.
run_named <- function(nm, est_method = "dr") {
  d <- collision_data()
  d[[nm[["gname"]]]] <- d$grp
  d[[nm[["tname"]]]] <- d$year
  d[[nm[["idname"]]]] <- d$unit
  d[[nm[["yname"]]]] <- d$y
  set.seed(123)
  suppressWarnings(pte_default(
    yname = nm[["yname"]], gname = nm[["gname"]], tname = nm[["tname"]],
    idname = nm[["idname"]], data = d, d_outcome = TRUE, xformula = ~x,
    est_method = est_method, biters = 100
  ))
}

summarize_res <- function(r) {
  list(
    att = r$att_gt$att, se = r$att_gt$se,
    group = r$att_gt$group, t = r$att_gt$t,
    overall = c(r$overall_att$overall.att, r$overall_att$overall.se),
    egt = r$event_study$egt, es = r$event_study$att.egt,
    es_se = r$event_study$se.egt
  )
}

plain <- c(gname = "grp", tname = "year", idname = "unit", yname = "y")
colliding <- list(
  gname_G = c(gname = "G", tname = "year", idname = "unit", yname = "y"),
  tname_period = c(gname = "grp", tname = "period", idname = "unit",
                   yname = "y"),
  tname_G = c(gname = "grp", tname = "G", idname = "unit", yname = "y"),
  idname_id = c(gname = "grp", tname = "year", idname = "id", yname = "y"),
  yname_Y = c(gname = "grp", tname = "year", idname = "unit", yname = "Y"),
  all = c(gname = "G", tname = "period", idname = "id", yname = "Y")
)

test_that("internal column names give the same results as other names", {
  for (method in c("dr", "reg")) {
    expected <- summarize_res(run_named(plain, method))
    # sanity check: years are mapped back and the overall ATT is finite
    expect_equal(sort(unique(expected$group)), c(2003, 2004))
    expect_true(is.finite(expected$overall[1]))
    for (case in names(colliding)) {
      got <- summarize_res(run_named(colliding[[case]], method))
      expect_equal(got, expected, info = paste(method, case))
    }
  }
})

test_that("setup_pte() leaves the user's columns unchanged", {
  d <- collision_data()
  d$G <- d$grp
  d$period <- d$year
  ptep <- setup_pte(yname = "y", gname = "G", tname = "period",
                    idname = "unit", data = d)
  # user's columns keep their calendar-year values
  expect_setequal(unique(ptep$data$G), c(0, 2003, 2004))
  expect_setequal(unique(ptep$data$period), 2001:2005)
  # internal columns are recoded to 1, ..., T
  expect_setequal(unique(ptep$data$.G), c(0, 3, 4))
  expect_setequal(unique(ptep$data$.period), 1:5)
})

test_that("gt_data_frame() gives cells the plain internal names", {
  d <- collision_data()
  ptep <- setup_pte(yname = "y", gname = "grp", tname = "year",
                    idname = "unit", data = d)
  cell <- two_by_two_subset(ptep$data, g = 3, tp = 4)$gt_data
  expect_s3_class(cell, "gt_data_frame")
  expect_equal(cell$G, cell$.G)
  expect_equal(cell$period, cell$.period)
  expect_equal(cell$id, cell$.id)
  expect_equal(cell$Y, cell$.Y)
  expect_setequal(unique(cell$period), c(2, 4))
})

test_that("all subset functions return gt_data_frame cells", {
  d <- collision_data()
  ptep <- setup_pte(yname = "y", gname = "grp", tname = "year",
                    idname = "unit", data = d)
  for (sf in list(two_by_two_subset, keep_all_untreated_subset,
                  keep_all_pretreatment_subset)) {
    cell <- sf(ptep$data, g = 3, tp = 4)$gt_data
    expect_s3_class(cell, "gt_data_frame")
    expect_true(all(c("G", "id", "period", "Y", "name", "D") %in%
                      colnames(cell)))
  }
})

test_that("gt_data_frame() errors clearly without the internal columns", {
  old_style <- data.frame(G = 1, id = 1, period = 1, Y = 1, name = "pre",
                          D = 0)
  expect_error(gt_data_frame(old_style), "\\.G, \\.id, \\.period, \\.Y")
})
