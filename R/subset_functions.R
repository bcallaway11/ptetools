# =============================================================================
# Title: Subset Functions
# Description: Functions that build the data for a single (g,t) cell:
#   two_by_two_subset (panel), two_by_two_rcs_subset (repeated cross
#   sections), keep_all_untreated_subset and keep_all_pretreatment_subset.
# Author: Brant Callaway
# Last update: 2026-11-14
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
  # get the correct "base" period for this group
  main.base.period <- g - anticipation - 1

  #----------------------------------------------------
  if (base_period == "varying") {
    # if it's a pre-treatment time period (used for the
    # pre-test, we need to adjust the base period)

    # group not treated yet
    if (tp < (g - anticipation)) {
      # move to earlier period
      # not going to include anticipation here
      base.period <- tp - 1
    } else {
      # this is a post-treatment period
      base.period <- main.base.period
    }
  } else {
    base.period <- main.base.period
  }
  #----------------------------------------------------

  #----------------------------------------------------
  # collect the right subset of the data: group g plus the comparison group
  # (not-yet-treated or never-treated), in the current and base periods
  this.data <- gt_cell_data(data, g, tp, base.period, control_group)

  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$period == tp, "post", "pre")

  # variable to indicate local treatment status
  this.data$D <- 1 * (this.data$G == g)

  # make this.data into gt_data_frame object
  this.data <- gt_data_frame(this.data)

  # number of observations used for this (g,t)
  n1 <- length(unique(this.data$id))
  all_ids <- attr(data, "pte_ids")
  if (is.null(all_ids)) all_ids <- unique(data$id)
  disidx <- all_ids %in% unique(this.data$id)

  list(gt_data = this.data, n1 = n1, disidx = disidx)
}

# --- Internal: rows of a 2x2 cell ----------------------------------------
# Returns the rows of `data` for group g and its comparison group in periods
# tp and base.period, in their original order (the same rows as
# subset(data, <group condition> & <period condition>)).
#
# When `data` comes from setup_pte(), it carries the row indices of each
# period ("pte_period_rows"), so only the rows of the two periods are
# examined and copied; otherwise this falls back to scanning all rows.
gt_cell_data <- function(data, g, tp, base.period, control_group) {
  period_rows <- attr(data, "pte_period_rows")
  if (!is.null(period_rows) && sum(lengths(period_rows)) == nrow(data)) {
    rows <- sort(c(period_rows[[as.character(tp)]],
                   period_rows[[as.character(base.period)]]))
    G <- data$G[rows]
    in_cell <- if (control_group == "notyettreated") {
      G == g | G > tp | G == 0
    } else {
      G == g | G == 0
    }
    # which() drops NA, as subset() does
    return(data[rows[which(in_cell)], , drop = FALSE])
  }

  if (control_group == "notyettreated") {
    this.data <- subset(data, G == g | G > tp | G == 0)
  } else {
    # use never treated group
    this.data <- subset(data, G == g | G == 0)
  }
  subset(this.data, period == tp | period == base.period)
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
  # get the correct "base" period for this group
  main.base.period <- g - anticipation - 1

  if (base_period == "varying") {
    if (tp < (g - anticipation)) {
      base.period <- tp - 1
    } else {
      base.period <- main.base.period
    }
  } else {
    base.period <- main.base.period
  }

  this.data <- gt_cell_data(data, g, tp, base.period, control_group)
  this.data$name <- ifelse(this.data$period == tp, "post", "pre")
  this.data$D <- 1 * (this.data$G == g)
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
  data$.treat <- 1 * ((data$G <= data$period) & (data$G != 0))
  this.data <- subset(data, (.treat == 0) | (G == g))

  # This line drops any periods after the "current" period too (even
  # if these are untreated) for group g.
  # This is to deal with pre-treatment periods
  # where it seems ambiguous/confusing what exactly to do and rules
  # out that later periods will be used to estimate the model for untreated
  # potential outcomes using group g.
  this.data <- subset(this.data, !(G == g & period > tp))

  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$period == tp, "post", "pre")

  this.data$D <- 1 * ((this.data$G == g) & this.data$period >= tp)
  n1 <- length(unique(this.data$id))
  disidx <- unique(data$id) %in% unique(this.data$id)

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
  this.data <- subset(data, period <= tp)
  # keep group g, not-yet-treated groups, and never-treated group
  this.data <- subset(this.data, (G == g) | (G > tp) | (G == 0))

  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$period == tp, "post", "pre")

  this.data$D <- 1 * ((this.data$G == g) & this.data$period >= tp)
  n1 <- length(unique(this.data$id))
  disidx <- unique(data$id) %in% unique(this.data$id)

  list(gt_data = this.data, n1 = n1, disidx = disidx)
}
