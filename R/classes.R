# =============================================================================
# Title: ptetools S3 Classes
# Description: S3 class constructors and print/summary methods for all major
#   result types: group_time_att, pte_results, pte_emp_boot, pte_qtt,
#   pte_dose_results, attgt_if, attgt_noif, gt_data_frame.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2021-05-17
# =============================================================================

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
  out$DIDparams$panel <- out$ptep$panel
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
  cband <- isTRUE(object$ptep$cband)
  cband_text1a <- paste0(100 * (1 - alp), "% ")
  cband_text1b <- ifelse(cband, "Simult. ", "Pointwise ")
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
  print(summary(x, ...), ...)
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
#' @description Turns the rows selected for one group-time cell into the
#'  data that an \code{attgt_fun} receives, and sets its class to include
#'  \code{gt_data_frame}. Every \code{subset_fun} should finish by calling it.
#'
#'  Setup functions such as [setup_pte()] store their internal columns under
#'  dot-prefixed names (\code{.G}, \code{.id}, \code{.period}, \code{.Y}), so
#'  they never overwrite the user's own columns. \code{gt_data_frame()}
#'  copies these to the plain names \code{G}, \code{id}, \code{period} and
#'  \code{Y} that \code{attgt_fun}s use. The \code{subset_fun} must already
#'  have added \code{name} (\code{"pre"}/\code{"post"}) and the local
#'  treatment indicator \code{D}.
#'
#' @param data rows of the full data (from a setup function) for one
#'  group-time cell, including the columns \code{.G}, \code{.id},
#'  \code{.period}, \code{.Y}, \code{name} and \code{D}
#'
#' @return \code{gt_data_frame} object, with columns \code{G}, \code{id},
#'  \code{period}, \code{Y}, \code{name} and \code{D} in addition to the
#'  columns of \code{data}
#'
#' @export
gt_data_frame <- function(data) {
  internal_cols <- c(".G", ".id", ".period", ".Y")
  missing_cols <- setdiff(internal_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(
      "gt_data_frame() needs the columns ",
      paste(internal_cols, collapse = ", "),
      ", created by setup_pte(); missing: ",
      paste(missing_cols, collapse = ", "),
      ". A custom setup function must create these dot-prefixed columns.",
      call. = FALSE
    )
  }

  # plain names for attgt functions; inside a cell these copies may replace
  # user columns with the same name, which is harmless because aggregation
  # reads the user's columns from the full data, not from the cell
  data$G <- data$.G
  data$id <- data$.id
  data$period <- data$.period
  data$Y <- data$.Y

  if (!all(c("name", "D") %in% colnames(data))) {
    warning("group-time subset of data does not contain columns name and D")
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
#'  corresponding to deliver averaged group-specific treatment effects.
#'  All weight vectors from \code{attgt_pte_aggregations()} are in the order
#'  of the group-time cells as computed (\code{attgt.list}, and so
#'  \code{extra_gt_returns}), with 0 for cells dropped as \code{NA}; this is
#'  not necessarily the row order of \code{attgt_results}.
#'
#' @param ptep \code{pte_params} object, stored for reference in the result.
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
                         extra_gt_returns = NULL,
                         ptep = NULL) {
  out <- list(
    attgt_results = attgt_results,
    overall_results = overall_results,
    group_results = group_results,
    dyn_results = dyn_results,
    overall_weights = overall_weights,
    dyn_weights = dyn_weights,
    group_weights = group_weights,
    extra_gt_returns = extra_gt_returns,
    ptep = ptep
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
  overall_att  <- object$overall_results$att
  overall_se   <- object$overall_results$se
  overall_cval <- object$overall_results$crit_val

  event_study_att <- object$dyn_results$att.e
  event_study_se  <- object$dyn_results$se
  event_study_e   <- object$dyn_results$e

  alp <- if (!is.null(object$ptep$alp)) object$ptep$alp else 0.05
  pointwise_cval <- qnorm(1 - alp / 2)
  use_cval <- if (!is.null(overall_cval)) overall_cval else pointwise_cval
  overall_cband_upper <- overall_att + use_cval * overall_se
  overall_cband_lower <- overall_att - use_cval * overall_se
  out1 <- cbind.data.frame(overall_att, overall_se, overall_cband_lower, overall_cband_upper)
  out1 <- round(out1, 4)
  overall_sig <- (overall_cband_upper < 0) | (overall_cband_lower > 0)
  overall_sig[is.na(overall_sig)] <- FALSE
  overall_sig_text <- ifelse(overall_sig, "*", "")
  out1 <- cbind.data.frame(out1, overall_sig_text)


  # Event study header
  cband <- isTRUE(object$ptep$cband)
  cband_text1a <- paste0(100 * (1 - alp), "% ")
  cband_text1b <- ifelse(cband, "Simult. ", "Pointwise ")
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
    cband = cband
  )

  class(out) <- "summary.pte_results"

  out
}


#' @title Class for QTT Curve Results
#'
#' @description Holds the full quantile treatment effect (QTT) curve at the
#'  overall, group-specific, and dynamic (event-study) aggregation levels.
#'  Each aggregation contains estimates at all quantile levels in \code{probs}
#'  together with bootstrap standard errors and pointwise confidence intervals.
#'
#' @param overall data.frame with columns \code{probs}, \code{qtt}, \code{se},
#'  \code{lower_pw}, \code{upper_pw}, \code{lower_ub}, \code{upper_ub}
#' @param dynamic data.frame with columns \code{e}, \code{probs}, \code{qtt},
#'  \code{se}, \code{lower_pw}, \code{upper_pw}, \code{lower_ub}, \code{upper_ub}
#' @param group data.frame with columns \code{group}, \code{probs}, \code{qtt},
#'  \code{se}, \code{lower_pw}, \code{upper_pw}, \code{lower_ub}, \code{upper_ub}
#' @param att \code{NULL}, or a list with the \code{overall},
#'  \code{dynamic} and \code{group} ATTs from the same run and bootstrap
#'  draws: data.frames with columns (\code{e} or \code{group},)
#'  \code{att}, \code{se}, \code{lower}, \code{upper} (pointwise)
#' @param qtt_minus_att \code{NULL}, or a list with the \code{overall},
#'  \code{dynamic} and \code{group} curves QTT(tau) - ATT, with the same
#'  columns as the QTT results (value column \code{qtt_minus_att}) plus
#'  \code{pval}: the bootstrap sup-t p-value of the test that QTT(tau) =
#'  ATT for every tau in \code{probs}, one per curve (repeated on its rows)
#' @param F0_overall mixture CDF of untreated potential outcomes
#' @param F1_overall mixture CDF of treated potential outcomes
#' @param ptep \code{pte_params} object
#'
#' @return a \code{pte_qtt} object
#'
#' @export
pte_qtt <- function(overall,
                    dynamic,
                    group,
                    F0_overall = NULL, # nolint: object_name_linter
                    F1_overall = NULL, # nolint: object_name_linter
                    ptep,
                    att = NULL,
                    qtt_minus_att = NULL) {
  out <- list(
    overall       = overall,
    dynamic       = dynamic,
    group         = group,
    att           = att,
    qtt_minus_att = qtt_minus_att,
    F0_overall    = F0_overall, # nolint: object_name_linter
    F1_overall    = F1_overall, # nolint: object_name_linter
    ptep          = ptep
  )
  class(out) <- "pte_qtt"
  out
}


#' @title summary.pte_qtt
#'
#' @description Summary for a \code{pte_qtt} object: the overall QTT curve,
#'  and the event-study and group QTTs at selected quantiles. When the
#'  object holds the ATTs from the same run (\code{qtt_pte_aggregations()}),
#'  also the overall and event-study ATTs, with the p-value of the test that
#'  QTT(tau) = ATT at every estimated quantile. Confidence bands
#'  are uniform over quantiles (for each curve) when \code{cband = TRUE} was
#'  used for estimation, and pointwise otherwise.
#'
#' @param object a \code{pte_qtt} object
#' @param probs quantile levels at which to report the event-study and group
#'  QTTs (default \code{0.5}, the median). Must be among the estimated
#'  quantiles.
#' @param ... additional arguments
#'
#' @return a \code{summary.pte_qtt} object, printed by its \code{print}
#'  method
#' @export
summary.pte_qtt <- function(object, probs = 0.5, ...) {
  alp <- if (!is.null(object$ptep$alp)) object$ptep$alp else 0.05
  cband <- isTRUE(object$ptep$cband)
  lower_col <- if (cband) "lower_ub" else "lower_pw"
  upper_col <- if (cband) "upper_ub" else "upper_pw"

  # one table: key columns, QTT, SE, band, and "*" if the band excludes 0
  qtt_table <- function(df, keys, key_names) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    lower <- df[[lower_col]]
    upper <- df[[upper_col]]
    sig <- (upper < 0) | (lower > 0)
    sig[is.na(sig)] <- FALSE
    out <- data.frame(df[keys], df$qtt, df$se, lower, upper)
    out[] <- lapply(out, function(v) round(v, 4))
    colnames(out) <- c(key_names, "QTT", "Std. Error", "Lower", "Upper")
    out$sig <- ifelse(sig, "*", "")
    # sorted by the keys (e.g. event time, then quantile)
    out <- out[do.call(order, unname(as.list(df[keys]))), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  # event-study and group QTTs at the requested quantiles
  at_probs <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    available <- unique(df$probs)
    near <- function(x, y) vapply(x, function(v) any(abs(v - y) < 1e-8),
                                  logical(1))
    keep <- near(df$probs, probs)
    missing_p <- probs[!near(probs, available)]
    if (length(missing_p) > 0) {
      stop("probs value(s) not among the estimated quantiles: ",
           paste(missing_p, collapse = ", "), call. = FALSE)
    }
    df[keep, , drop = FALSE]
  }
  dyn <- at_probs(object$dynamic)
  grp <- at_probs(object$group)

  # ATTs from the same run, with the p-value of the test that QTT(tau) =
  # ATT for every tau (#27): overall, then by event time
  att_table <- function(object) {
    if (is.null(object$att) || is.null(object$qtt_minus_att)) return(NULL)
    pval_of <- function(df, key, k) {
      if (is.null(key)) df$pval[1] else df$pval[df[[key]] == k][1]
    }
    a_o <- object$att$overall
    a_d <- object$att$dynamic
    rows <- data.frame(
      Parameter = c("Overall", paste("Event time", a_d$e)),
      ATT = c(a_o$att, a_d$att),
      se = c(a_o$se, a_d$se),
      lower = c(a_o$lower, a_d$lower),
      upper = c(a_o$upper, a_d$upper),
      pval = c(pval_of(object$qtt_minus_att$overall, NULL),
               vapply(a_d$e, function(k) {
                 pval_of(object$qtt_minus_att$dynamic, "e", k)
               }, numeric(1)))
    )
    rows[-1] <- lapply(rows[-1], function(v) round(v, 4))
    colnames(rows) <- c("Parameter", "ATT", "Std. Error", "Lower", "Upper",
                        "p (QTT = ATT)")
    rows
  }
  one_prob <- length(probs) == 1

  out <- list(
    overall = qtt_table(object$overall, "probs", "Quantile"),
    dynamic = if (one_prob) {
      qtt_table(dyn, "e", "Event Time")
    } else {
      qtt_table(dyn, c("e", "probs"), c("Event Time", "Quantile"))
    },
    group = if (one_prob) {
      qtt_table(grp, "group", "Group")
    } else {
      qtt_table(grp, c("group", "probs"), c("Group", "Quantile"))
    },
    att = att_table(object),
    probs = probs,
    alp = alp,
    cband = cband
  )
  class(out) <- "summary.pte_qtt"
  out
}


#' @title print.summary.pte_qtt
#'
#' @description Prints a \code{summary.pte_qtt} object.
#'
#' @param x a \code{summary.pte_qtt} object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return \code{x}, invisibly
#' @export
print.summary.pte_qtt <- function(x, ...) {
  band <- paste0("[", 100 * (1 - x$alp), "% ",
                 if (x$cband) "Simult. " else "Pointwise ", "Conf. Band]")
  print_table <- function(title, tab) {
    if (is.null(tab)) return(invisible(NULL))
    cat(title, "\n", sep = "")
    n <- ncol(tab)
    colnames(tab)[n - 2] <- band
    colnames(tab)[n - 1] <- ""
    colnames(tab)[n] <- ""
    print(tab, row.names = FALSE)
    cat("\n")
  }
  at <- if (length(x$probs) == 1) {
    paste0(" (quantile ", x$probs, ")")
  } else {
    ""
  }
  cat("\n")
  print_table("Overall QTT curve:", x$overall)
  print_table(paste0("Dynamic effects", at, ":"), x$dynamic)
  print_table(paste0("Group effects", at, ":"), x$group)
  if (!is.null(x$att)) {
    cat("ATT, and test of QTT(tau) = ATT for all quantiles:\n")
    tab <- x$att
    colnames(tab)[4] <- paste0("[", 100 * (1 - x$alp), "% Pointwise")
    colnames(tab)[5] <- "Conf. Int.]"
    print(tab, row.names = FALSE)
    cat("\n")
  }
  cat("---\n")
  cat("Signif. codes: `*' confidence band does not cover 0\n")
  if (x$cband) {
    cat("Simultaneous bands are uniform over quantiles, for each QTT curve.\n")
  }
  if (!is.null(x$att)) {
    cat("p (QTT = ATT): bootstrap sup-t test, from the same draws, that",
        "QTT(tau) - ATT = 0\nat every estimated quantile.\n")
  }
  invisible(x)
}


#' @title print.pte_qtt
#'
#' @description Prints a \code{pte_qtt} object.
#'
#' @param x a \code{pte_qtt} object
#' @param ... extra arguments passed to \code{summary.pte_qtt()}
#'
#' @keywords internal
#' @return \code{x}, invisibly
#' @export
print.pte_qtt <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
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
