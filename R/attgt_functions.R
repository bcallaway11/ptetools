# =============================================================================
# Title: ATT(g,t) Functions
# Description: Estimators for a single (g,t) cell (did_attgt, did_rcs_attgt,
#   pte_attgt) and internal helpers that put a 2x2 panel cell in wide form.
# Author: Brant Callaway
# Last update: 2026-11-20
# Date created: 2021-05-18
# =============================================================================

# --- Internal: est_method names (#30) ---------------------------------------
# "aipw" (augmented inverse probability weighting, i.e. doubly robust) is
# the name of the doubly robust estimator; "dr" is a deprecated alias for it.
# "dr" was ambiguous in this family of packages (qte uses cond_dist = "dr"
# for distribution regression). The deprecation warning is given once per
# session, since the attgt functions are called once per (g,t) cell.
pte_env <- new.env(parent = emptyenv())

pte_est_method <- function(est_method) {
  if (!(is.character(est_method) && length(est_method) == 1 &&
          est_method %in% c("aipw", "reg", "dr"))) {
    stop("est_method must be \"aipw\" (doubly robust) or \"reg\" ",
         "(outcome regression).", call. = FALSE)
  }
  if (est_method == "dr") {
    if (!isTRUE(pte_env$warned_dr)) {
      warning("est_method = \"dr\" is deprecated; use \"aipw\", the same ",
              "doubly robust estimator.", call. = FALSE)
      pte_env$warned_dr <- TRUE
    }
    est_method <- "aipw"
  }
  est_method
}

# --- Internal: 2x2 panel cell in wide form -------------------------------
# Splits a panel gt_data_frame into its "pre" and "post" rows, with the post
# rows reordered to match the pre rows by id. Everything per-unit (outcomes,
# covariates, weights, D) is then taken from rows in the same order, whatever
# the order of gt_data (#9). Units are in the order of the pre rows, which
# for data from setup_pte() is the order of unique(data$.id), as compute.pte()
# expects for the influence function.
gt_pre_post <- function(gt_data) {
  pre <- gt_data[gt_data$name == "pre", , drop = FALSE]
  post <- gt_data[gt_data$name == "post", , drop = FALSE]
  list(pre = pre, post = post[match(pre$id, post$id), , drop = FALSE])
}

# Outcomes in wide form: one row per unit with columns id, D, post, pre
# (the layout previously produced by dcast(id + D ~ name)).
gt_wide_outcomes <- function(pp) {
  data.frame(id = pp$pre$id, D = pp$pre$D, post = pp$post$Y, pre = pp$pre$Y)
}

# Covariate matrix for a 2x2 cell, one row per unit (in the order of the pre
# rows, as gt_wide_outcomes()): the design matrix of `xformula` evaluated on
# the pre-period rows, plus the change between periods in the variables of
# `d_covs_formula` and, optionally, the pre-period (lagged) outcome.
#
# The design matrix is built in one step from the original variables, so
# terms that transform them, such as factor(), I() or log(), work. These
# failed when the matrix was rebuilt from the names of already-transformed
# columns (#5). Like before, an intercept is always included.
#
# Arguments: pp (from gt_pre_post()), xformula, d_covs_formula (~-1 for
#   none), lagged_outcome_cov (logical).
# Returns: numeric matrix.
gt_covariates <- function(pp, xformula, d_covs_formula = ~-1,
                          lagged_outcome_cov = FALSE) {
  pre <- droplevels(pp$pre)
  # na.pass keeps one row per unit, aligned with the outcomes
  mf <- stats::model.frame(xformula, data = pre, na.action = stats::na.pass)
  covmat <- stats::model.matrix(attr(mf, "terms"), data = mf)
  if (!("(Intercept)" %in% colnames(covmat))) {
    covmat <- cbind("(Intercept)" = 1, covmat)
  }

  # change in covariates between the post and pre periods
  d_frame <- function(d) {
    stats::model.frame(d_covs_formula, data = d, na.action = stats::na.pass)
  }
  dX <- d_frame(pp$post) - d_frame(pp$pre)
  if (ncol(dX) > 0) {
    colnames(dX) <- paste0("d", colnames(dX))
    covmat <- cbind(covmat, as.matrix(dX))
  }

  if (isTRUE(lagged_outcome_cov)) covmat <- cbind(covmat, pre = pp$pre$Y)
  covmat
}

#' @title Difference-in-differences for ATT(g,t)
#'
#' @description Takes a data.frame and computes for a particular group g
#'  and time period t and computes
#'  an estimate of a group time average treatment effect
#'  and a corresponding influence function using a difference in differences
#'  approach.
#'
#'  The code relies on \code{gt_data} having certain variables defined.
#'  In particular, there should be an \code{id} column (individual identifier),
#'  \code{D} (treated group identifier), \code{period} (time period), \code{name}
#'  (equal to "pre" for pre-treatment periods and equal to "post" for post
#'  treatment periods), \code{Y} (outcome).
#'
#'  In our case, we call \code{two_by_two_subset} which sets up the
#'  data to have this format before the call to \code{did_attgt}.
#'
#'  Sampling weights (\code{weightsname} in \code{pte()}, stored as
#'  \code{.w}) are passed to \code{DRDID::drdid_panel()}.
#'
#' @param gt_data data that is "local" to a particular group-time average
#'  treatment effect
#' @param xformula one-sided formula for covariates used in the propensity score
#'  and outcome regression models
#' @param ... extra function arguments; not used here
#'
#' @return attgt_if
#'
#' @export
did_attgt <- function(gt_data, xformula = ~1, ...) {
  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------
  # pre and post rows, matched by unit
  pp <- gt_pre_post(gt_data)

  # outcomes and pre-treatment covariates (one row per unit)
  gt_dataX <- gt_wide_outcomes(pp)
  covmat <- gt_covariates(pp, xformula)

  # treatment dummy variable
  D <- gt_dataX$D

  # pre- and post-treatment outcomes
  Y_post <- gt_dataX$post
  Y_pre <- gt_dataX$pre

  # call DRDID functions to make the computations;
  # just like in `did` package. Sampling weights (.w, all 1 without
  # weightsname) were not passed, so they were ignored (#21); normalized to
  # mean 1 within the cell, as DRDID does anyway.
  w <- pp$pre$.w
  if (is.null(w)) w <- rep(1, length(Y_post))
  attgt <- DRDID::drdid_panel(
    y1 = Y_post,
    y0 = Y_pre,
    D = D,
    covariates = covmat,
    i.weights = w / mean(w),
    inffunc = TRUE
  )

  # return attgt
  attgt_if(attgt = attgt$ATT, inf_func = attgt$att.inf.func)
}

#' @title Repeated Cross Sections Difference-in-Differences for ATT(g,t)
#'
#' @description Takes a local repeated cross sections data set and computes
#'  an estimate of a group-time average treatment effect and corresponding
#'  influence function using a repeated cross sections DID approach.
#'
#' @inheritParams did_attgt
#' @param est_method Which type of estimation method to use. The default is
#'  "aipw" (augmented inverse probability weighting, i.e. doubly robust).
#'  The other option is "reg" for regression adjustment. "dr" is a
#'  deprecated alias for "aipw".
#'
#' @return attgt_if
#'
#' @export
did_rcs_attgt <- function(gt_data, xformula = ~1, est_method = "aipw", ...) {
  est_method <- pte_est_method(est_method)
  gt_data <- droplevels(gt_data)

  y <- gt_data$Y
  post <- 1 * (gt_data$name == "post")
  D <- gt_data$D
  covariates <- model.matrix(xformula, data = gt_data)
  i.weights <- gt_data$.w / mean(gt_data$.w)

  if (est_method == "aipw") {
    attgt <- DRDID::drdid_rc(
      y = y,
      post = post,
      D = D,
      covariates = covariates,
      i.weights = i.weights,
      inffunc = TRUE
    )
  } else if (est_method == "reg") {
    attgt <- DRDID::reg_did_rc(
      y = y,
      post = post,
      D = D,
      covariates = covariates,
      i.weights = i.weights,
      inffunc = TRUE
    )
  }

  attgt_if(attgt = attgt$ATT, inf_func = attgt$att.inf.func)
}


#' @title General ATT(g,t)
#'
#' @description `pte_attgt` takes a "local" data.frame and computes
#'  an estimate of a group time average treatment effect
#'  and a corresponding influence function.  This function generalizes
#'  a number of existing methods and underlies the `pte_default` function.
#'
#'  The code relies on \code{gt_data} having certain variables defined.
#'  In particular, there should be an \code{id} column (individual identifier),
#'  \code{G} (group identifier), \code{period} (time period), \code{name}
#'  (equal to "pre" for pre-treatment periods and equal to "post" for post
#'  treatment periods), \code{Y} (outcome).
#'
#'  In our case, we call \code{two_by_two_subset} which sets up the
#'  data to have this format before the call to `pte_attgt`
#'
#' @param gt_data data that is "local" to a particular group-time average
#'  treatment effect
#' @param xformula one-sided formula for covariates used in the propensity score
#'  and outcome regression models
#' @param d_outcome Whether or not to take the first difference of the outcome.
#'  The default is FALSE.  To use difference-in-differences, set this to be TRUE.
#' @param d_covs_formula A formula for time varying covariates to enter the
#'  first estimation step models.  The default is not to include any, and, hence,
#'  to only include pre-treatment covariates.
#' @param lagged_outcome_cov Whether to include the lagged outcome as a covariate.
#'  Default is FALSE.
#' @param est_method Which type of estimation method to use. The default is
#'  "aipw" (augmented inverse probability weighting, i.e. doubly robust).
#'  The other option is "reg" for regression adjustment. "dr" is a
#'  deprecated alias for "aipw".
#' @param ... extra function arguments; not used here
#'
#' @return attgt_if
#'
#' @export
pte_attgt <- function(
    gt_data,
    xformula,
    d_outcome = FALSE,
    d_covs_formula = ~ -1, lagged_outcome_cov = FALSE,
    est_method = "aipw",
    ...) {
  est_method <- pte_est_method(est_method)
  # pre and post rows, matched by unit
  pp <- gt_pre_post(gt_data)

  this.g <- unique(pp$post$G[pp$post$D == 1])
  this.tp <- unique(pp$post$period)

  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------

  # pre-treatment covariates, changes in covariates and (optionally) the
  # lagged outcome, one row per unit
  covmat <- gt_covariates(pp, xformula, d_covs_formula, lagged_outcome_cov)
  .w <- pp$pre$.w

  # outcomes (one row per unit)
  gt_dataX <- gt_wide_outcomes(pp)

  # treatment dummy variable
  D <- gt_dataX$D

  # post treatment outcome
  Y <- gt_dataX$post

  if (d_outcome) Y <- gt_dataX$post - gt_dataX$pre

  # estimate attgt
  # DRDID::drdid_panel is for panel data, but we can hack it
  # to work in levels by just setting outcomes in "first period"
  # to be equal to 0 for all units
  covmat2 <- covmat[D == 0, ]
  n_unt <- sum(1 - D)
  precheck_reg <- qr(t(covmat2) %*% covmat2 / n_unt)
  keep_covs <- precheck_reg$pivot[1:precheck_reg$rank]
  covmat <- covmat[, keep_covs]

  # if group is too small, switch to reg adjustment
  if (est_method == "aipw") {
    # Overlap check: logit propensity scores. quasibinomial() fits the same
    # model with the same algorithm (identical fitted values) but, unlike
    # binomial(), does not warn "non-integer #successes" for non-integer
    # weights; only fitted values are used here (#20). Weights normalized to
    # mean 1; the fitted values do not depend on their scale.
    pscore_est <- glm(D ~ covmat, family = quasibinomial(link = "logit"),
                      weights = .w / mean(.w))
    pscore <- predict(pscore_est, type = "response")
    if (max(pscore) > 0.99) {
      est_method <- "reg"
      # say which problem it is: with no more treated units than covariates
      # the logit separates whatever the overlap; otherwise some treated
      # units have no comparable comparison units (#28)
      n_treated <- sum(D)
      reason <- if (n_treated <= ncol(covmat)) {
        paste0("too few treated units (", n_treated, ") for ",
               ncol(covmat), " covariates")
      } else {
        share <- mean(pscore[D == 1] > 0.99)
        paste0("the propensity score is above 0.99 for ",
               round(100 * share, 1), "% of treated units (limited ",
               "overlap; see check_overlap())")
      }
      warning("Switching to regression adjustment for group ", this.g,
              " in time period ", this.tp, ": ", reason, ".", call. = FALSE)
    }
  }

  if (est_method == "aipw") {
    attgt <- DRDID::drdid_panel(
      y1 = Y,
      y0 = rep(0, length(Y)),
      D = D,
      covariates = covmat,
      i.weights = .w / mean(.w), # DRDID rescales to mean 1 anyway
      inffunc = TRUE
    )
  } else if (est_method == "reg") {
    attgt <- DRDID::reg_did_panel(
      y1 = Y,
      y0 = rep(0, length(Y)),
      D = D,
      covariates = covmat,
      i.weights = .w / mean(.w), # DRDID rescales to mean 1 anyway
      inffunc = TRUE
    )
  }
  # return attgt
  attgt_if(attgt = attgt$ATT, inf_func = attgt$att.inf.func)
}
