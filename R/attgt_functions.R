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
  # for outcome regression, get pre-treatment values
  Xpre <- model.frame(xformula, data = subset(gt_data, name == "pre"))

  # convert two period panel into one period
  gt_data_outcomes <- tidyr::pivot_wider(gt_data[, c("D", "id", "period", "name", "Y")],
    id_cols = c(id, D),
    names_from = c(name),
    values_from = c(Y)
  )

  # merge outcome and covariate data
  gt_dataX <- cbind.data.frame(gt_data_outcomes, Xpre)

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
  this.g <- subset(gt_data, name == "post" & D == 1)$G
  this.tp <- unique(subset(gt_data, name == "post")$period)

  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------

  # pre-treatment covariates
  Xpre <- model.frame(xformula, data = subset(gt_data, name == "pre"))
  .w <- subset(gt_data, name == "pre")$.w

  # change in covariates
  dX <- model.frame(d_covs_formula, data = subset(gt_data, name == "post")) - model.frame(d_covs_formula, data = subset(gt_data, name == "pre"))
  if (ncol(dX) > 0) colnames(dX) <- paste0("d", colnames(dX))

  # convert two period panel into one period
  gt_data_outcomes <- tidyr::pivot_wider(gt_data[, c("D", "id", "period", "name", "Y")],
    id_cols = c(id, D),
    names_from = c(name),
    values_from = c(Y)
  )

  # merge outcome and covariate data
  gt_dataX <- cbind.data.frame(gt_data_outcomes, Xpre, dX, .w)

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
  use_formula <- BMisc::toformula("", c(BMisc::rhs.vars(xformula), colnames(dX)))
  if (lagged_outcome_cov) use_formula <- BMisc::addCovToFormla("pre", use_formula)
  covmat <- model.matrix(use_formula, data = gt_dataX)
  covmat2 <- covmat[D == 0, ]
  # www <- gt_dataX[D==0,]$.w
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
    # } else if (est_method == "grf") {
    #   # sampling weights not supported here
    #   # code requires custom version of grf package
    #   tau.forest <- causal_forest(X = covmat, Y = Y, W = D)
    #   # predict(tau.forest)$predictions[D==1]

    #   grf_res <- average_treatment_effect(tau.forest, method = "AIPW", target.sample = "treated")
    #   this_n <- nrow(covmat)
    #   this_if <- as.matrix(grf_res$inf_func * this_n)
    #   # V <- t(this_if) %*% this_if / this_n
    #   # sqrt(V) / sqrt(this_n)
    #   # V <- t(grf_res$inf_func/sqrt(this_n)) %*% grf_res$inf_func/sqrt(this_n)
    #   attgt <- list(ATT = grf_res$estimate, att.inf.func = this_if)
    # } else if (est_method == "lasso") {
    #   # code adapted from: https://thomaswiemann.com/ddml/articles/did.html
    #   learners <- list(what = ddml::mdl_glmnet)
    #   learners_DX <- learners

    #   att_fit <- ddml::ddml_att(
    #     y = Y,
    #     D = D,
    #     X = covmat,
    #     learners = learners,
    #     learners_DX = learners_DX,
    #     sample_folds = 10,
    #     silent = TRUE
    #   )
    #   inf.func <- att_fit$psi_b + att_fit$att * att_fit$psi_a
    #   attgt <- list(ATT = att_fit$att, att.inf.func = inf.func)
  } else {
    stop(paste0("est_method: ", est_method, " is not supported"))
  }
  # return attgt
  attgt_if(attgt = attgt$ATT, inf_func = attgt$att.inf.func)
}
