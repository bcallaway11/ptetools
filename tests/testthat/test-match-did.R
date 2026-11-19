# =============================================================================
# Title: Tests that pte_default() matches did::att_gt()
# Description: ATT(g,t), event study and overall ATT from pte_default() must
#   equal did::att_gt() for both base periods and both control groups, on
#   mpdta and on unequally spaced periods. Covers #16: with a universal base
#   period, not-yet-treated controls must be untreated in the base period
#   (#16); aggregated standard errors (#19) and the pre-test (#18) must
#   match too;
#   anticipation follows did's rules, in units of the time variable.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

# max absolute differences between pte_default() and did::att_gt()
diff_from_did <- function(data, yname, gname, tname, idname, base_period,
                          control_group, anticipation = 0) {
  a <- suppressWarnings(pte_default(
    yname = yname, gname = gname, tname = tname, idname = idname,
    data = data, d_outcome = TRUE, base_period = base_period,
    control_group = control_group, anticipation = anticipation, biters = 0
  ))
  d <- suppressMessages(suppressWarnings(did::att_gt(
    yname = yname, gname = gname, tname = tname, idname = idname,
    data = data, base_period = base_period, control_group = control_group,
    anticipation = anticipation, bstrap = FALSE, cband = FALSE
  )))
  did_att <- stats::setNames(d$att, paste(d$group, d$t))
  dyn <- did::aggte(d, type = "dynamic")
  grp <- did::aggte(d, type = "group")
  e_match <- match(a$event_study$egt, dyn$egt)
  c(
    attgt = max(abs(a$att_gt$att -
                      did_att[paste(a$att_gt$group, a$att_gt$t)])),
    event_study = max(abs(a$event_study$att.egt - dyn$att.egt[e_match])),
    overall = abs(a$overall_att$overall.att - grp$overall.att),
    # standard errors of aggregated parameters (#19); the event-study SE at
    # a universal reference period is NA in both packages
    event_study_se = max(abs(a$event_study$se.egt - dyn$se.egt[e_match]),
                         na.rm = TRUE),
    overall_se = abs(a$overall_att$overall.se - grp$overall.se),
    # Wald pre-test, also with a universal base period (#18)
    pretest = abs(as.numeric(a$att_gt$W) - d$W)
  )
}

settings <- expand.grid(base_period = c("varying", "universal"),
                        control_group = c("notyettreated", "nevertreated"),
                        stringsAsFactors = FALSE)

test_that("pte_default() matches did::att_gt() on mpdta", {
  skip_if_not_installed("did")
  for (i in seq_len(nrow(settings))) {
    diffs <- diff_from_did(did::mpdta, "lemp", "first.treat", "year",
                           "countyreal", settings$base_period[i],
                           settings$control_group[i])
    expect_lt(max(diffs), 1e-10, label = paste(settings[i, ], collapse = "/"))
  }
})

test_that("pte_default() matches did::att_gt() with unequally spaced periods", {
  skip_if_not_installed("did")
  set.seed(1)
  n <- 1500
  years <- c(2001, 2002, 2005, 2010)
  grp <- sample(c(0, 2005, 2010), n, replace = TRUE)
  d <- data.frame(id = rep(seq_len(n), each = 4), year = rep(years, n),
                  grp = rep(grp, each = 4))
  d$y <- rnorm(nrow(d)) + 0.1 * (d$year - 2001) +
    ifelse(d$grp > 0 & d$year >= d$grp, 1 + 0.2 * (d$year - d$grp), 0)
  for (i in seq_len(nrow(settings))) {
    diffs <- diff_from_did(d, "y", "grp", "year", "id",
                           settings$base_period[i], settings$control_group[i])
    expect_lt(max(diffs), 1e-10, label = paste(settings[i, ], collapse = "/"))
  }
})

# --- Anticipation -------------------------------------------------------
# Anticipation is in units of the time variable, as in did::att_gt(). did
# 2.5.1 (CRAN) ignores anticipation when deciding which cells are
# post-treatment, so for anticipation > 0 the expected values here come
# from did's rules computed by hand; the comparison with did itself only
# runs for later versions.

# ATT(g,t) by hand: simple 2x2 DiD of mean outcome changes (no covariates),
# with the base period and comparison group defined as in did::att_gt()
hand_attgt <- function(data, yname, gname, tname, idname, anticipation,
                       base_period, control_group) {
  periods <- sort(unique(data[[tname]]))
  unit <- data[!duplicated(data[[idname]]), c(idname, gname)]
  groups <- sort(setdiff(unique(unit[[gname]]), 0))
  wide <- stats::reshape(data[, c(idname, tname, yname)], idvar = idname,
                         timevar = tname, direction = "wide")
  wide <- merge(wide, unit, by = idname)
  out <- NULL
  for (g in groups) {
    # groups with no period early enough to be the base period are dropped
    if (!any(periods + anticipation < g)) next
    base_g <- max(periods[periods + anticipation < g])
    for (tp in periods[-1]) {
      post <- tp >= g - anticipation
      base <- if (base_period == "varying" && !post) {
        max(periods[periods < tp])
      } else {
        base_g
      }
      if (base == tp) next
      dy <- wide[[paste0(yname, ".", tp)]] - wide[[paste0(yname, ".", base)]]
      gv <- wide[[gname]]
      ctrl <- if (control_group == "nevertreated") {
        gv == 0
      } else {
        gv == 0 | (gv != g & gv > max(tp, base) + anticipation)
      }
      out <- rbind(out, data.frame(
        g = g, t = tp, att = mean(dy[gv == g]) - mean(dy[ctrl])
      ))
    }
  }
  out
}

unequal_data <- function() {
  set.seed(1)
  n <- 1500
  years <- c(2001, 2002, 2005, 2010)
  grp <- sample(c(0, 2005, 2010), n, replace = TRUE)
  d <- data.frame(id = rep(seq_len(n), each = 4), year = rep(years, n),
                  grp = rep(grp, each = 4))
  d$y <- rnorm(nrow(d)) +
    ifelse(d$grp > 0 & d$year >= d$grp, 1, 0)
  d
}

test_that("anticipation follows did::att_gt()'s rules, in units of tname", {
  skip_if_not_installed("did")
  cases <- list(
    list(data = did::mpdta, y = "lemp", g = "first.treat", t = "year",
         id = "countyreal", ant = c(1, 2)),
    list(data = unequal_data(), y = "y", g = "grp", t = "year", id = "id",
         ant = c(1, 3))
  )
  for (cs in cases) for (ant in cs$ant) for (i in seq_len(nrow(settings))) {
    bp <- settings$base_period[i]
    cg <- settings$control_group[i]
    res <- suppressWarnings(pte_default(
      yname = cs$y, gname = cs$g, tname = cs$t, idname = cs$id,
      data = cs$data, d_outcome = TRUE, est_method = "reg",
      anticipation = ant, base_period = bp, control_group = cg, biters = 0
    ))
    expected <- hand_attgt(cs$data, cs$y, cs$g, cs$t, cs$id, ant, bp, cg)
    got <- res$att_gt$att[match(paste(expected$g, expected$t),
                                paste(res$att_gt$group, res$att_gt$t))]
    expect_equal(got, expected$att, tolerance = 1e-10,
                 info = paste(cs$y, "anticipation", ant, bp, cg))
  }
})

test_that("pte_default() matches did::att_gt() with anticipation", {
  skip_if_not_installed("did")
  skip_if(utils::packageVersion("did") <= "2.5.1",
          "did <= 2.5.1 ignores anticipation for post-treatment cells")
  for (ant in c(1, 2)) {
    diffs <- diff_from_did(did::mpdta, "lemp", "first.treat", "year",
                           "countyreal", "varying", "notyettreated",
                           anticipation = ant)
    expect_lt(max(diffs), 1e-10)
  }
})

test_that("units first treated after the sample are never treated", {
  skip_if_not_installed("did")
  late <- did::mpdta
  late$first.treat[late$first.treat == 2007] <- 2012
  never <- did::mpdta
  never$first.treat[never$first.treat == 2007] <- 0
  run <- function(d) {
    suppressWarnings(pte_default(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = d, d_outcome = TRUE, biters = 0
    ))
  }
  a <- run(late)
  b <- run(never)
  expect_equal(a$att_gt$att, b$att_gt$att)
  expect_equal(a$overall_att$overall.se, b$overall_att$overall.se)
})

test_that("no group with a base period gives a clear error", {
  skip_if_not_installed("did")
  expect_error(
    suppressWarnings(pte_default(
      yname = "lemp", gname = "first.treat", tname = "year",
      idname = "countyreal", data = did::mpdta, d_outcome = TRUE,
      anticipation = 5, biters = 0
    )),
    "No treated group has a base period"
  )
})

# --- No never-treated group -------------------------------------------------
# As in did::att_gt(), the latest cohort serves as the comparison group and
# periods from its treatment date on are dropped. setup_pte() used to drop
# the earliest cohort instead (it removed the smallest group value, assuming
# it was 0).
test_that("pte_default() matches did::att_gt() without never-treated units", {
  skip_if_not_installed("did")
  data <- did::mpdta[did::mpdta$first.treat != 0, ]
  for (i in seq_len(nrow(settings))) {
    diffs <- suppressWarnings(diff_from_did(
      data, "lemp", "first.treat", "year", "countyreal",
      settings$base_period[i], settings$control_group[i]
    ))
    expect_lt(max(diffs, na.rm = TRUE), 1e-10,
              label = paste(settings[i, ], collapse = "/"))
  }
})

test_that("no comparison units at all gives a clear error", {
  set.seed(1)
  n <- 100
  d <- data.frame(id = rep(seq_len(n), each = 2), period = rep(1:2, n),
                  grp = 2)
  d$y <- rnorm(nrow(d))
  expect_error(
    pte_default(yname = "y", gname = "grp", tname = "period", idname = "id",
                data = d, biters = 0),
    "no comparison units"
  )
})

test_that("require_comparison = FALSE allows no comparison units", {
  # every unit treated in one of two periods, none untreated: estimators
  # that use only within-group variation (contdid's ACRT) need setup to
  # keep every group (contdid #5)
  set.seed(1)
  n <- 50
  d <- data.frame(id = rep(seq_len(n), each = 3), period = rep(1:3, n),
                  grp = rep(c(2, 3), length.out = n)[rep(seq_len(n), each = 3)])
  d$y <- rnorm(nrow(d))
  ptep <- setup_pte(yname = "y", gname = "grp", tname = "period",
                    idname = "id", data = d, require_comparison = FALSE)
  expect_setequal(ptep$glist, c(2, 3))
  expect_false(ptep$require_comparison)
  # by default, without never-treated units the latest cohort is the
  # comparison group (as in did)
  expect_equal(setup_pte(yname = "y", gname = "grp", tname = "period",
                         idname = "id", data = d)$glist, 2)
})

test_that("an invalid control_group gives an error", {
  skip_if_not_installed("did")
  # any other value used to be treated as never treated, silently
  expect_error(
    pte_default(yname = "lemp", gname = "first.treat", tname = "year",
                idname = "countyreal", data = did::mpdta,
                control_group = "notyettreatd", biters = 0),
    "control_group must be"
  )
})
