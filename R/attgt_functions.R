# =============================================================================
# Title: ATT(g,t) Functions
# Description: Estimators for a single (g,t) cell (did_attgt, did_rcs_attgt,
#   pte_attgt) and internal helpers that put a 2x2 panel cell in wide form.
# Author: Brant Callaway
# Last update: 2026-11-14
# Date created: 2021-05-18
# =============================================================================

# --- Internal: 2x2 panel cell in wide form -------------------------------
# Splits a panel gt_data_frame into its "pre" and "post" rows, with the post
# rows reordered to match the pre rows by id. Everything per-unit (outcomes,
# covariates, weights, D) is then taken from rows in the same order, whatever
# the order of gt_data (#9). Units are in the order of the pre rows, which
# for data from setup_pte() is the order of unique(data$id), as compute.pte()
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

  # for outcome regression, get pre-treatment values
  Xpre <- model.frame(xformula, data = pp$pre)

  # merge outcome and covariate data (one row per unit)
  gt_dataX <- cbind.data.frame(gt_wide_outcomes(pp), Xpre)

  # treatment dummy variable
  D <- gt_dataX$D

  # pre- and post-treatment outcomes
  Y_post <- gt_dataX$post
  Y_pre <- gt_dataX$pre

  # call DRDID functions to make the computations;
  # just like in `did` package
  gt_dataX <- droplevels(gt_dataX)
  attgt <- DRDID::drdid_panel(
    y1 = Y_post,
    y0 = Y_pre,
    D = D,
    covariates = model.matrix(xformula,
      data = gt_dataX
    ),
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
#' @param est_method Which type of estimation method to use. Default is "dr" for
#'  doubly robust.  The other option is "reg" for regression adjustment.
#'
#' @return attgt_if
#'
#' @export
did_rcs_attgt <- function(gt_data, xformula = ~1, est_method = "dr", ...) {
  gt_data <- droplevels(gt_data)

  y <- gt_data$Y
  post <- 1 * (gt_data$name == "post")
  D <- gt_data$D
  covariates <- model.matrix(xformula, data = gt_data)
  i.weights <- gt_data$.w / mean(gt_data$.w)

  if (est_method == "dr") {
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
  } else {
    stop(paste0("est_method: ", est_method, " is not supported"))
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
#' @param est_method Which type of estimation method to use. Default is "dr" for
#'  doubly robust.  The other option is "reg" for regression adjustment.
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
    est_method = "dr",
    ...) {
  # pre and post rows, matched by unit
  pp <- gt_pre_post(gt_data)

  this.g <- pp$post$G[pp$post$D == 1]
  this.tp <- unique(pp$post$period)

  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------

  # pre-treatment covariates
  Xpre <- model.frame(xformula, data = pp$pre)
  .w <- pp$pre$.w

  # change in covariates
  dX <- model.frame(d_covs_formula, data = pp$post) - model.frame(d_covs_formula, data = pp$pre)
  if (ncol(dX) > 0) colnames(dX) <- paste0("d", colnames(dX))

  # merge outcome and covariate data (one row per unit)
  gt_dataX <- cbind.data.frame(gt_wide_outcomes(pp), Xpre, dX, .w)

  # treatment dummy variable
  D <- gt_dataX$D

  # post treatment outcome
  Y <- gt_dataX$post

  if (d_outcome) Y <- gt_dataX$post - gt_dataX$pre

  # estimate attgt
  # DRDID::drdid_panel is for panel data, but we can hack it
  # to work in levels by just setting outcomes in "first period"
  # to be equal to 0 for all units
  gt_dataX <- droplevels(gt_dataX)
  use_formula <- BMisc::toformula("", c(BMisc::rhs_vars(xformula), colnames(dX)))
  if (lagged_outcome_cov) use_formula <- BMisc::add_cov_to_formula("pre", use_formula)
  covmat <- model.matrix(use_formula, data = gt_dataX)
  covmat2 <- covmat[D == 0, ]
  n_unt <- sum(1 - D)
  precheck_reg <- qr(t(covmat2) %*% covmat2 / n_unt)
  keep_covs <- precheck_reg$pivot[1:precheck_reg$rank]
  covmat <- covmat[, keep_covs]

  # if group is too small, switch to reg adjustment
  if (est_method == "dr") {
    pscore_est <- glm(D ~ covmat, family = binomial(link = "logit"), weights = (.w / sum(.w)))
    pscore <- predict(pscore_est, type = "response")
    if (max(pscore) > 0.99) {
      est_method <- "reg"
      warning(paste0(
        "Switching to regression adjustment because of small group size for group ",
        this.g, " in time period ", this.tp
      ))
    }
  }

  if (est_method == "dr") {
    attgt <- DRDID::drdid_panel(
      y1 = Y,
      y0 = rep(0, length(Y)),
      D = D,
      covariates = covmat,
      i.weights = (.w / sum(.w)),
      inffunc = TRUE
    )
  } else if (est_method == "reg") {
    attgt <- DRDID::reg_did_panel(
      y1 = Y,
      y0 = rep(0, length(Y)),
      D = D,
      covariates = covmat,
      i.weights = (.w / sum(.w)),
      inffunc = TRUE
    )
  } else {
    stop(paste0("est_method: ", est_method, " is not supported"))
  }
  # return attgt
  attgt_if(attgt = attgt$ATT, inf_func = attgt$att.inf.func)
}
