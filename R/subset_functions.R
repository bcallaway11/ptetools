# =============================================================================
# Title: Subset Functions
# Description: Functions that build the data for a single (g,t) cell:
#   two_by_two_subset (panel), two_by_two_rcs_subset (repeated cross
#   sections), keep_all_untreated_subset and keep_all_pretreatment_subset.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2021-05-17
# =============================================================================

#' @title Two Period Two Group Subset
#'
#' @description A function for computing a 2x2 subset of original data.
#'   This is the subset with post treatment periods separately for the
#'   treated group and comparison group and pre-treatment periods in the period
#'   immediately before the treated group became treated.
#'
#' @inheritParams pte_params
#'
#' @param data the full dataset
#' @param g the current group
#' @param tp the current time period
#' @param control_group whether to use "notyettreated" (default) or
#'  "nevertreated"
#' @param anticipation the number of periods of anticipation (i.e., number
#'  of periods before the treatment happens where the treatment can "already"
#'  affect the outcome)
#' @param ... extra arguments to get the subset correct
#'
#' @return list that contains the following elements:
#'   - `gt_data`: a `gt_data_frame` object that contains the
#'        correct subset of data
#'   - `n1`: the number of observations in this subset
#'   - `disidx`: a vector of the correct ids for this subset
#'
#' @export
two_by_two_subset <- function(data,
                              g,
                              tp,
                              control_group = "notyettreated",
                              anticipation = 0,
                              base_period = "varying",
                              ...) {
  # base period for this cell: the period just before tp in pre-treatment
  # cells with a varying base period, otherwise the last period at least
  # `anticipation` time units before g (as in did::att_gt())
  base.period <- pte_base_period(data, g, tp, anticipation, base_period)

  # collect the right subset of the data: group g plus the comparison group
  # (not-yet-treated or never-treated), in the current and base periods
  this.data <- gt_cell_data(data, g, tp, base.period, control_group,
                            anticipation)

  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$.period == tp, "post", "pre")

  # variable to indicate local treatment status
  this.data$D <- 1 * (this.data$.G == g)

  # make this.data into gt_data_frame object
  this.data <- gt_data_frame(this.data)

  # number of observations used for this (g,t)
  n1 <- length(unique(this.data$.id))
  all_ids <- attr(data, "pte_ids")
  if (is.null(all_ids)) all_ids <- unique(data$.id)
  disidx <- all_ids %in% unique(this.data$.id)

  list(gt_data = this.data, n1 = n1, disidx = disidx)
}

# --- Internal: calendar time ---------------------------------------------
# setup_pte() recodes time periods and groups to 1..T (.period, .G) and keeps
# the original time periods as attribute "pte_time_periods" of the data.
# Anticipation and base periods are defined in units of the time variable
# (calendar time), as in did::att_gt(), so comparisons that involve
# anticipation map period indices back to calendar time here. Data from
# setups that do not recode periods (setup_pte_basic()) have no attribute;
# their periods already are calendar time.
#
# Arguments: data (full data from a setup function), p (period indices;
#   0, the never-treated group, maps to NA).
# Returns: calendar times of p.
pte_calendar_time <- function(data, p) {
  cal <- attr(data, "pte_time_periods")
  if (is.null(cal)) {
    return(ifelse(p > 0, p, NA_real_))
  }
  out <- rep(NA_real_, length(p))
  pos <- !is.na(p) & p > 0
  out[pos] <- cal[p[pos]]
  out
}

# Returns the base period (a period index) for the cell of group g in period
# tp, as in did::att_gt():
#   - post-treatment cells (t >= g - anticipation, in calendar time) and all
#     cells with a universal base period use the last period p with
#     p + anticipation < g;
#   - pre-treatment cells with a varying base period use the period just
#     before tp.
# NA if group g has no period early enough to serve as its base period.
pte_base_period <- function(data, g, tp, anticipation = 0,
                            base_period = "varying") {
  cal <- attr(data, "pte_time_periods")
  periods <- if (is.null(cal)) sort(unique(data$.period)) else seq_along(cal)
  cal_periods <- pte_calendar_time(data, periods)
  cal_g <- pte_calendar_time(data, g)

  post <- pte_calendar_time(data, tp) >= cal_g - anticipation
  candidates <- if (base_period == "varying" && !post) {
    periods[periods < tp]
  } else {
    periods[cal_periods + anticipation < cal_g]
  }
  if (length(candidates) == 0) NA else max(candidates)
}

# --- Internal: rows of a 2x2 cell ----------------------------------------
# Returns the rows of `data` for group g and its comparison group in periods
# tp and base.period, in their original order (the same rows as
# subset(data, <group condition> & <period condition>)).
#
# Not-yet-treated comparison units must be untreated, and not yet
# anticipating treatment, in *both* periods of the cell: units first treated
# more than `anticipation` time units after max(tp, base.period), as in
# did::att_gt(). With a universal base period, the base period comes after
# tp in pre-treatment cells; selecting on G > tp alone would use units that
# are already treated in the base period as controls (#16).
#
# When `data` comes from setup_pte(), it carries the row indices of each
# period ("pte_period_rows"), so only the rows of the two periods are
# examined and copied; otherwise this falls back to scanning all rows.
gt_cell_data <- function(data, g, tp, base.period, control_group,
                         anticipation = 0) {
  threshold <- pte_calendar_time(data, max(tp, base.period)) + anticipation
  in_group <- function(G) {
    if (control_group == "notyettreated") {
      # NA calendar time for G == 0 is harmless: G == 0 is TRUE there
      G == g | G == 0 | pte_calendar_time(data, G) > threshold
    } else {
      # use never treated group
      G == g | G == 0
    }
  }

  period_rows <- attr(data, "pte_period_rows")
  if (!is.null(period_rows) && sum(lengths(period_rows)) == nrow(data)) {
    rows <- sort(c(period_rows[[as.character(tp)]],
                   period_rows[[as.character(base.period)]]))
    # which() drops NA, as subset() does
    return(data[rows[which(in_group(data$.G[rows]))], , drop = FALSE])
  }

  in_period <- data$.period == tp | data$.period == base.period
  data[which(in_group(data$.G) & in_period), , drop = FALSE]
}

#' @title Two Period Two Group Repeated Cross Sections Subset
#'
#' @description A function for computing a 2x2 subset for repeated cross
#' sections data.  This is analogous to \code{two_by_two_subset}, but indexes
#' observations by rows rather than by panel ids.
#'
#' @inheritParams two_by_two_subset
#'
#' @return list that contains the following elements:
#'   - `gt_data`: a `gt_data_frame` object that contains the
#'        correct subset of data
#'   - `n1`: the number of observations in this subset
#'   - `disidx`: a vector of the correct rows for this subset
#'
#' @export
two_by_two_rcs_subset <- function(data,
                                  g,
                                  tp,
                                  control_group = "notyettreated",
                                  anticipation = 0,
                                  base_period = "varying",
                                  ...) {
  # base period as in two_by_two_subset()
  base.period <- pte_base_period(data, g, tp, anticipation, base_period)

  this.data <- gt_cell_data(data, g, tp, base.period, control_group,
                            anticipation)
  this.data$name <- ifelse(this.data$.period == tp, "post", "pre")
  this.data$D <- 1 * (this.data$.G == g)
  this.data <- gt_data_frame(this.data)

  n1 <- nrow(this.data)
  disidx <- data$.rowid %in% this.data$.rowid

  list(gt_data = this.data, n1 = n1, disidx = disidx)
}


#' @title Keep All Untreated Subset
#'
#' @description A function that takes an original data set and keeps all
#' pre-treatment data for all groups.  For group g, it also includes data
#' for the current period.
#'
#' Also, note that if `tp` is still a pre-treatment period for group g,
#' then periods after `tp` will also be dropped for group g.  This is a
#' design choice and is useful especially for estimating placebo
#' group-time average treatment effects in pre-treatment periods.
#'
#' A main use case for this function is to compute ATT(g,t)'s using a global
#' estimation strategy such as imputation in Gardner (2022).
#'
#' @inheritParams two_by_two_subset
#'
#' @return list that contains the following elements:
#'   - `gt_data`: a `gt_data_frame` object that contains the
#'        correct subset of data
#'   - `n1`: the number of observations in this subset
#'   - `disidx`: a vector of the correct ids for this subset
#'
#' @export
keep_all_untreated_subset <- function(data, g, tp, ...) {
  # drop post-treatment observations that are not in group g
  # this creates the same sort of unbalanced panel data set
  # used in imputation papers.
  G <- data$.G
  period <- data$.period
  treated_now <- (G <= period) & (G != 0)
  keep <- !treated_now | (G == g)

  # This line drops any periods after the "current" period too (even
  # if these are untreated) for group g.
  # This is to deal with pre-treatment periods
  # where it seems ambiguous/confusing what exactly to do and rules
  # out that later periods will be used to estimate the model for untreated
  # potential outcomes using group g.
  keep <- keep & !(G == g & period > tp)
  this.data <- data[which(keep), , drop = FALSE]

  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$.period == tp, "post", "pre")

  this.data$D <- 1 * ((this.data$.G == g) & this.data$.period >= tp)
  this.data <- gt_data_frame(this.data)
  n1 <- length(unique(this.data$.id))
  disidx <- unique(data$.id) %in% unique(this.data$.id)

  list(gt_data = this.data, n1 = n1, disidx = disidx)
}


#' @title Keep All Pre-Treatment Subset
#'
#' @description A function that takes an original data set and keeps all
#' data for all groups that are not-yet-treated by period `tp` as well
#' as for group `g`.
#'
#' In particular, this keeps more data than functions like `two_by_two`
#' subset that use a fixed base period.
#'
#' A main use case for this function is the interactive fixed effects approach
#' proposed in Callaway and Tsyawo (2023).
#'
#' @inheritParams two_by_two_subset
#' @param ... additional arguments
#'
#' @return list that contains the following elements:
#'   - `gt_data`: a `gt_data_frame` object that contains the
#'     correct subset of data
#'   - `n1`: the number of observations in this subset
#'   - `disidx`: a vector of the correct ids for this subset
#'
#' @export
keep_all_pretreatment_subset <- function(data, g, tp, ...) {
  G <- data$.G
  # periods up to tp; group g, not-yet-treated groups, and never-treated group
  keep <- (data$.period <= tp) & ((G == g) | (G > tp) | (G == 0))
  this.data <- data[which(keep), , drop = FALSE]

  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$.period == tp, "post", "pre")

  this.data$D <- 1 * ((this.data$.G == g) & this.data$.period >= tp)
  this.data <- gt_data_frame(this.data)
  n1 <- length(unique(this.data$.id))
  disidx <- unique(data$.id) %in% unique(this.data$.id)

  list(gt_data = this.data, n1 = n1, disidx = disidx)
}
