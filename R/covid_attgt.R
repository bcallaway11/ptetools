# =============================================================================
# Title: Covid ATT(g,t) Estimator
# Description: Unconfoundedness-based ATT(g,t) estimator for pandemic policy
#   evaluation (Callaway and Li 2021). Moved here from the ppe package.
# Author: Brant Callaway
# Last update: 2026-05-25
# Date created: 2021-05-14
# =============================================================================

#' @title Covid ATT(g,t) Estimator
#'
#' @description Computes a group-time average treatment effect and influence
#'   function using an unconfoundedness-type identification strategy.  This
#'   estimator is appropriate when parallel trends is implausible but a
#'   selection-on-observables assumption holds in levels (rather than
#'   differences) — e.g., during the early COVID-19 pandemic.
#'
#'   Originally from Callaway and Li (2021). Moved into \code{ptetools} from
#'   the \code{ppe} package.
#'
#' @param gt_data data that is "local" to a particular group-time average
#'   treatment effect, structured as a \code{gt_data_frame}
#' @param xformla one-sided formula for covariates used in the propensity
#'   score and outcome regression models
#' @param d_outcome logical; if \code{TRUE}, use first-differenced outcomes.
#'   Default is \code{FALSE} (levels).
#' @param d_covs_formula one-sided formula for covariates to include as
#'   changes (differences). Default is \code{~-1} (no change covariates).
#' @param ... extra arguments; not used
#'
#' @return \code{attgt_if} object
#'
#' @references Callaway, B. and Li, T. (2021). Policy Evaluation during a
#'   Pandemic. \url{https://arxiv.org/abs/2105.06927}
#'
#' @export
covid_attgt <- function(gt_data, xformla, d_outcome = FALSE,
                        d_covs_formula = ~-1, ...) {
  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------

  # pre-treatment covariates
  Xpre <- model.frame(xformla, data = subset(gt_data, name == "pre"))

  # change in covariates
  dX <- model.frame(d_covs_formula, data = subset(gt_data, name == "post")) -
    model.frame(d_covs_formula, data = subset(gt_data, name == "pre"))
  if (ncol(dX) > 0) colnames(dX) <- paste0("d", colnames(dX))

  # convert two-period panel into one period (wide format)
  gt_data_outcomes <- data.table::dcast(
    data.table::as.data.table(gt_data[, c("D", "id", "period", "name", "Y")]),
    id + D ~ name, value.var = "Y"
  )

  # merge outcome and covariate data
  gt_dataX <- cbind.data.frame(gt_data_outcomes, Xpre, dX)

  # treatment dummy variable
  D <- gt_dataX$D

  # outcome: levels or first differences
  Y <- gt_dataX$post
  if (d_outcome) Y <- gt_dataX$post - gt_dataX$pre

  #-----------------------------------------------------------------------------
  # estimate ATT(g,t) via DRDID
  # drdid_panel is for panel data; setting y0 = 0 adapts it to the levels case
  #-----------------------------------------------------------------------------
  gt_dataX <- droplevels(gt_dataX)
  use_formula <- BMisc::toformula("", c(BMisc::rhs_vars(xformla), colnames(dX)))
  covmat <- model.matrix(use_formula, data = gt_dataX)

  # drop collinear columns among untreated units
  covmat2 <- covmat[D == 0, ]
  n_unt <- sum(1 - D)
  precheck_reg <- qr(t(covmat2) %*% covmat2 / n_unt)
  keep_covs <- precheck_reg$pivot[seq_len(precheck_reg$rank)]
  covmat <- covmat[, keep_covs]

  attgt <- DRDID::drdid_panel(
    y1 = Y,
    y0 = rep(0, length(Y)),
    D = D,
    covariates = covmat,
    inffunc = TRUE
  )

  attgt_if(attgt = attgt$ATT, inf_func = attgt$att.inf.func)
}
