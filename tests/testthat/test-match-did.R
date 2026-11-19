# =============================================================================
# Title: Tests that pte_default() matches did::att_gt()
# Description: ATT(g,t), event study and overall ATT from pte_default() must
#   equal did::att_gt() for both base periods and both control groups, on
#   mpdta and on unequally spaced periods. Covers #16: with a universal base
#   period, not-yet-treated controls must be untreated in the base period.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

# max absolute differences between pte_default() and did::att_gt()
diff_from_did <- function(data, yname, gname, tname, idname, base_period,
                          control_group) {
  a <- suppressWarnings(pte_default(
    yname = yname, gname = gname, tname = tname, idname = idname,
    data = data, d_outcome = TRUE, base_period = base_period,
    control_group = control_group, biters = 0
  ))
  d <- suppressWarnings(did::att_gt(
    yname = yname, gname = gname, tname = tname, idname = idname,
    data = data, base_period = base_period, control_group = control_group,
    bstrap = FALSE, cband = FALSE
  ))
  did_att <- stats::setNames(d$att, paste(d$group, d$t))
  dyn <- did::aggte(d, type = "dynamic")
  c(
    attgt = max(abs(a$att_gt$att -
                      did_att[paste(a$att_gt$group, a$att_gt$t)])),
    event_study = max(abs(a$event_study$att.egt -
                            dyn$att.egt[match(a$event_study$egt, dyn$egt)])),
    overall = abs(a$overall_att$overall.att -
                    did::aggte(d, type = "group")$overall.att)
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
