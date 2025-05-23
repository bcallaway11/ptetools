#' @title Class for Estimates across Groups and Time
#'
#' @description Class that holds causal effect parameter estimates across
#'  timing groups and time periods
#'
#' @param group numeric vector of groups for ATT(g,t)
#' @param time.period numeric vector of time periods for ATT(g,t)
#' @param att numeric vector containing the value of ATT(g,t) for
#'  corresponding group and time period
#' @param V_analytical analytical asymptotic variance matrix for ATT(g,t)'s
#' @param se numeric vector of standard errors
#' @param crit_val critical value (usually a critical value for conducting
#'  uniform inference)
#' @param inf_func matrix of influence function
#' @param n number of unique individuals
#' @param W Wald statistic for ATT(g,t) version of pre-test of parallel trends
#'  assumption
#' @param Wpval p-value for Wald pre-test of ATT(g,t) version of parallel
#'  trends assumption
#' @param cband logical indicating whether or not to report a confidence band
#' @param alp significance level
#' @param ptep \code{pte_params} object
#' @param extra_gt_returns list containing extra returns at the group-time level
#'
#' @return object of class \code{group_time_att}
#'
#' @export
group_time_att <- function(group,
                           time.period,
                           att,
                           V_analytical,
                           se,
                           crit_val,
                           inf_func,
                           n,
                           W,
                           Wpval,
                           cband,
                           alp,
                           ptep,
                           extra_gt_returns) {
  out <- list(
    group = group,
    t = time.period,
    att = att,
    V_analytical = V_analytical,
    se = se,
    crit_val = crit_val,
    inf_func = inf_func,
    n = n,
    W = W,
    Wpval = Wpval,
    cband = cband,
    alp = alp,
    ptep = ptep,
    extra_gt_returns = extra_gt_returns
  )

  # might eventually drop this, but add a few things to get access
  # to aggregations from did package
  out$DIDparams <- out$ptep
  out$DIDparams$panel <- TRUE
  out$DIDparams$data$w <- 1
  out$c <- out$crit_val
  out$inffunc <- out$inf_func


  class(out) <- c("group_time_att", "MP")

  out
}

#' @title summary.group_time_att
#'
#' @description prints a summary of a \code{group_time_att} object
#'
#' @param object an \code{group_time_att} object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return None. Prints a summary of the \code{group_time_att} object
#' @export
summary.group_time_att <- function(object, ...) {
  group_time_att_obj <- object

  # group time average treatment effects
  cat("Group-Time Average Treatment Effects:\n")

  cband_text1a <- paste0(100 * (1 - group_time_att_obj$alp), "% ")
  cband_text1b <- ifelse(group_time_att_obj$DIDparams$bstrap,
    ifelse(group_time_att_obj$DIDparams$cband, "Simult. ", "Pointwise "),
    "Pointwise "
  )
  cband_text1 <- paste0("[", cband_text1a, cband_text1b)

  cband_lower <- group_time_att_obj$att - group_time_att_obj$crit_val * group_time_att_obj$se
  cband_upper <- group_time_att_obj$att + group_time_att_obj$crit_val * group_time_att_obj$se

  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")

  out <- cbind.data.frame(group_time_att_obj$group, group_time_att_obj$t, group_time_att_obj$att, group_time_att_obj$se, cband_lower, cband_upper)
  out <- round(out, 4)
  out <- cbind.data.frame(out, sig_text)


  colnames(out) <- c("Group", "Time", "ATT(g,t)", "Std. Error", cband_text1, "Conf. Band]", "")
  print(out, row.names = FALSE)
  cat("---\n")
  cat("Signif. codes: `*' confidence band does not cover 0")
  cat("\n\n")

  # report pre-test
  if (!is.null(group_time_att_obj$Wpval)) {
    cat("P-value for pre-test of parallel trends assumption:  ")
    cat(as.character(group_time_att_obj$Wpval))
    cat("\n")
  }
}

#' @title print.group_time_att
#'
#' @description prints value of a \code{group_time_att} object
#'
#' @param x a \code{group_time_att} object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return None. Prints a summary of the \code{group_time_att} object
#' @export
print.group_time_att <- function(x, ...) {
  summary.group_time_att(x, ...)
}


#' @title Class for PTE Results
#'
#' @description Class for holding overall results with a staggered treatment,
#'  including an overall ATT and an event study
#'
#' @param att_gt attgt results
#' @param overall_att overall_att results
#' @param event_study event_study results
#' @param ptep \code{pte_params} object
#'
#' @return a `pte_results` object
#' @export
pte_results <- function(att_gt,
                        overall_att,
                        event_study,
                        ptep) {
  out <- list(
    att_gt = att_gt,
    overall_att = overall_att,
    event_study = event_study,
    ptep = ptep
  )

  class(out) <- "pte_results"

  out
}


#' @title summary.pte_results
#'
#' @description A function to summarize `ptetools` results.
#'
#' @param object an \code{pte_results} object
#' @param ... other arguments
#'
#' @keywords internal
#' @return a \code{summary.pte_results} object
#' @export
summary.pte_results <- function(object, ...) {
  overall_att <- object$overall_att$overall.att
  overall_se <- object$overall_att$overall.se

  event_study <- object$event_study
  event_study_att_egt <- object$event_study$att.egt
  event_study_se <- object$event_study$se.egt

  # overall estimates
  alp <- object$ptep$alp
  pointwise_cval <- qnorm(1 - alp / 2)
  overall_cband_upper <- overall_att + pointwise_cval * overall_se
  overall_cband_lower <- overall_att - pointwise_cval * overall_se
  out1 <- cbind.data.frame(overall_att, overall_se, overall_cband_lower, overall_cband_upper)
  out1 <- round(out1, 4)
  overall_sig <- (overall_cband_upper < 0) | (overall_cband_lower > 0)
  overall_sig[is.na(overall_sig)] <- FALSE
  overall_sig_text <- ifelse(overall_sig, "*", "")
  out1 <- cbind.data.frame(out1, overall_sig_text)


  # Event study
  # header
  bstrap <- object$DIDparams$bstrap
  cband <- object$DIDparams$cband
  cband_text1a <- paste0(100 * (1 - alp), "% ")
  cband_text1b <- ifelse(bstrap,
    ifelse(cband, "Simult. ", "Pointwise "),
    "Pointwise "
  )
  cband_text1 <- paste0("[", cband_text1a, cband_text1b)

  cband_lower <- event_study$att.egt - event_study$crit.val.egt * event_study$se.egt
  cband_upper <- event_study$att.egt + event_study$crit.val.egt * event_study$se.egt

  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")

  out2 <- cbind.data.frame(event_study$egt, event_study$att.egt, event_study$se.egt, cband_lower, cband_upper)
  out2 <- round(out2, 4)
  out2 <- cbind.data.frame(out2, sig_text)

  c1name <- "Event Time"
  colnames(out2) <- c(c1name, "Estimate", "Std. Error", cband_text1, "Conf. Band]", "")

  target_parameter <- object$ptep$target_parameter

  out <- list(
    overall_att = out1,
    event_study = out2,
    alp = alp,
    bstrap = bstrap,
    cband = cband,
    target_parameter = target_parameter
  )

  class(out) <- "summary.pte_results"

  out
}

#' @title print.pte_results
#'
#' @description prints value of a \code{pte_results} object
#'
#' @param x a \code{pte_results} object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return None. Prints a summary of the \code{pte_results} object
#' @export
print.pte_results <- function(x, ...) {
  # summary.pte_results(x,...)
  NextMethod(x)
  invisible(x)
}



#' @title print.summary.pte_results
#'
#' @description prints value of a \code{summary.pte_results} object
#'
#' @param x a \code{summary.pte_results} object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return None. Prints a summary of the \code{summary.pte_results} object
#' @export
print.summary.pte_results <- function(x, ...) {
  object <- x
  out1 <- object$overall_att
  out2 <- object$event_study
  alp <- object$alp
  target_parameter <- object$target_parameter

  # in a handful of cases, the overall parameter is not an ATT
  # (e.g., it could be an ACRT with a continuous treatment),
  # allow for possibility of difference here:
  att_text <- "Overall ATT" # default
  if (isTRUE(target_parameter == "slope")) {
    att_text <- "Overall ACRT"
  }

  # overall att
  cat("\n")
  cat(paste0(att_text, ":  \n"))
  colnames(out1) <- c("ATT", "   Std. Error", paste0("    [ ", 100 * (1 - alp), "% "), "Conf. Int.]", "")
  print(out1, row.names = FALSE)
  cat("\n\n")

  # event study
  c1name <- "Event time"
  cat("Dynamic Effects:")
  cat("\n")

  print(out2, row.names = FALSE, justify = "centre")

  cat("---\n")
  cat("Signif. codes: `*' confidence band does not cover 0")
  cat("\n\n")
}

#' @title Class for (g,t)-Specific Results with Influence Function
#'
#' @description Class for holding group-time average treatment effects
#'  along with their influence function
#'
#' @param attgt group-time average treatment effect
#' @param inf_func influence function
#' @param extra_gt_returns A place to return anything extra from particular
#'  group-time average treatment effect calculations.  For DID, this might
#'  be something like propensity score estimates, regressions of untreated
#'  potential outcomes on covariates.  For ife, this could be something
#'  like the first step regression 2sls estimates.  This argument is also
#'  potentially useful for debugging.
#'
#' @return `attgt_if` object
#'
#' @export
attgt_if <- function(attgt, inf_func, extra_gt_returns = NULL) {
  out <- list(attgt = attgt, inf_func = inf_func, extra_gt_returns = extra_gt_returns)
  class(out) <- "attgt_if"
  out
}

#' @title Class for (g,t)-Specific Results without Influence Function
#'
#' @description Class for holding returns from group-time specific estimates
#'  in settings when an influence function is not returned
#'
#' @inheritParams attgt_if
#'
#' @return an `attgt_noif` object
#'
#' @export
attgt_noif <- function(attgt, extra_gt_returns = NULL) {
  out <- list(attgt = attgt, extra_gt_returns = extra_gt_returns)
  class(out) <- "attgt_noif"
  out
}


#' @title Convert Data to Usable Format
#'
#' @description Checks and converts data to satisfy criteria to be used in internal
#'  `ptetools` functions.  In particular,
#'  the function takes in a data.frame, checks if it has the right
#'  columns to be used to calculate a group-time average treatment effect,
#'  and sets the class of the data.frame to include \code{gt_data_frame}
#'
#' @param data data that will be checked to see if has right format for
#'  computing group-time average treatment effects
#'
#' @return \code{gt_data_frame} object
#'
#' @export
gt_data_frame <- function(data) {
  cnames <- colnames(data)
  if (!all(c("G", "id", "period", "name", "Y", "D") %in% cnames)) {
    warning("group-time subset of data does not contain correct column names")
  }

  if (!("gt_data_frame" %in% class(data))) {
    class(data) <- c("gt_data_frame", class(data))
  }

  data
}

#' @title Class for Empirical Bootstrap Results
#'
#' @description Class for holding `ptetools` empirical bootstrap results
#'
#' @param attgt_results \code{data.frame} holding attgt results
#' @param overall_results \code{data.frame} holding overall results
#' @param group_results \code{data.frame} holding group results
#' @param dyn_results \code{data.frame} holding dynamic results
#' @param overall_weights vector containing weights on underlying ATT(g,t)
#'  for overall treatment effect parameter
#' @param dyn_weights list containing weights on underlying ATT(g,t)
#'  for each value of \code{e} corresponding to the dynamic treatment
#'  effect parameters.
#' @param group_weights list containing weights on underlying ATT(g,t)
#'  corresponding to deliver averaged group-specific treatment effects
#'
#' @inheritParams attgt_if
#'
#' @return a `pte_emp_boot` object
#'
#' @export
pte_emp_boot <- function(attgt_results,
                         overall_results,
                         group_results,
                         dyn_results,
                         overall_weights = NULL,
                         dyn_weights = NULL,
                         group_weights = NULL,
                         extra_gt_returns = NULL) {
  out <- list(
    attgt_results = attgt_results,
    overall_results = overall_results,
    group_results = group_results,
    dyn_results = dyn_results,
    overall_weights = overall_weights,
    dyn_weights = dyn_weights,
    group_weights = group_weights,
    extra_gt_returns = extra_gt_returns
  )

  class(out) <- "pte_emp_boot"

  out
}

#' @title summary.pte_emp_boot
#'
#' @description Summary for \code{pte_emp_boot} object
#'
#' @param object a \code{pte_emp_boot} object
#' @param ... additional function arguments
#'
#' @return \code{summary.pte_results} object
#'
#' @keywords internal
#' @export
summary.pte_emp_boot <- function(object, ...) {
  overall_att <- object$overall_results$att
  overall_se <- object$overall_results$se

  # event_study <- object$event_study
  event_study_att <- object$dyn_results$att.e
  event_study_se <- object$dyn_results$se
  event_study_e <- object$dyn_results$e

  # overall estimates
  alp <- .05 # hard coded
  pointwise_cval <- qnorm(1 - alp / 2)
  overall_cband_upper <- overall_att + pointwise_cval * overall_se
  overall_cband_lower <- overall_att - pointwise_cval * overall_se
  out1 <- cbind.data.frame(overall_att, overall_se, overall_cband_lower, overall_cband_upper)
  out1 <- round(out1, 4)
  overall_sig <- (overall_cband_upper < 0) | (overall_cband_lower > 0)
  overall_sig[is.na(overall_sig)] <- FALSE
  overall_sig_text <- ifelse(overall_sig, "*", "")
  out1 <- cbind.data.frame(out1, overall_sig_text)


  # Event study
  # header
  bstrap <- TRUE
  cband <- FALSE
  cband_text1a <- paste0(100 * (1 - alp), "% ")
  cband_text1b <- ifelse(bstrap,
    ifelse(cband, "Simult. ", "Pointwise "),
    "Pointwise "
  )
  cband_text1 <- paste0("[", cband_text1a, cband_text1b)

  cband_lower <- event_study_att - pointwise_cval * event_study_se
  cband_upper <- event_study_att + pointwise_cval * event_study_se

  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")

  out2 <- cbind.data.frame(event_study_e, event_study_att, event_study_se, cband_lower, cband_upper)
  out2 <- round(out2, 4)
  out2 <- cbind.data.frame(out2, sig_text)

  c1name <- "Event Time"
  colnames(out2) <- c(c1name, "Estimate", "Std. Error", cband_text1, "Conf. Band]", "")

  out <- list(
    overall_att = out1,
    event_study = out2,
    alp = alp,
    bstrap = bstrap,
    cband = cband
  )

  class(out) <- "summary.pte_results"

  out
}


#' @title Class for Continuous Treatment Results
#'
#' @description Class for holding results with a continuous treatment
#'
#' @param att_gt attgt results
#' @param dose vector of doses
#' @param att_d ATT(d) for each value of `dose`
#' @param acrt_d ACRT(d) for each value of `dose`
#' @param ptep a `pte_params` object
#'
#' @return a `pte_dose_results` object
#'
#' @export
pte_dose_results <- function(att_gt,
                             dose,
                             att_d = NULL,
                             acrt_d = NULL,
                             ptep) {
  out <- list(
    att_gt = att_gt,
    dose = dose,
    att_d = att_d,
    acrt_d = acrt_d,
    ptep = ptep
  )

  class(out) <- "pte_dose_results"

  out
}
