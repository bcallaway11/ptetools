# =============================================================================
# Title: Process ATT(g,t) Results
# Description: Processes ATT(g,t) results when an influence function is
#   available: analytical variance, multiplier bootstrap or analytical
#   standard errors, and the pre-treatment Wald test.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2021-05-14
# =============================================================================

#' @title Process ATT(g,t) Results
#'
#' @description Process ATT(g,t) results when influence function is available
#'
#' @param att_gt_results ATT(g,t)'s
#' @inheritParams pte_results
#'
#' @return `group_time_att` object
#'
#' @export
process_att_gt <- function(att_gt_results, ptep) {
  # extract ATT(g,t) and influence functions
  attgt.list <- att_gt_results$attgt.list
  inffunc <- att_gt_results$inffunc

  # process results
  attgt.results <- do.call("rbind.data.frame", attgt.list)
  att <- attgt.results$att
  group <- attgt.results$group
  time.period <- attgt.results$time.period
  extra_gt_returns <- att_gt_results$extra_gt_returns

  #-----------------------------------------------------------------------------
  # analytical standard errors
  #   * estimate variance
  #     this is analogous to cluster robust standard errors that
  #     are clustered at the unit level
  #-----------------------------------------------------------------------------
  #     (or at the level of clustervars: the variance uses sums of the
  #     influence function within clusters, #6)
  n <- nrow(inffunc)
  cluster <- pte_cluster(ptep)
  inffunc_sums <- cluster_sums(inffunc, cluster)
  V <- Matrix::t(inffunc_sums) %*% inffunc_sums / n
  se_analytical <- sqrt(Matrix::diag(V) / n)
  cband <- ptep$cband
  alp <- ptep$alp
  bstrap <- ptep$bstrap
  if (is.null(bstrap)) bstrap <- TRUE

  # critical value from N(0,1), for pointwise
  cval <- qnorm(1 - alp / 2)

  if (isTRUE(bstrap)) {
    # multiplier bootstrap results (biters was previously not passed, so
    # ATT(g,t) standard errors always used 1000 draws)
    biters <- if (is.null(ptep$biters)) 1000 else ptep$biters
    bout <- mboot2(inffunc, biters = biters, alp = alp, cband = cband,
                   cluster = cluster)
    se <- bout$boot_se
    if (cband && is.finite(bout$crit_val)) cval <- bout$crit_val
  } else {
    # purely analytical standard errors (clustered at the unit level, or
    # by clustervars);
    # only pointwise confidence intervals are supported for this option
    se <- se_analytical
  }

  #-----------------------------------------------------------------------------
  # compute Wald pre-test
  #-----------------------------------------------------------------------------
  # select which periods are pre-treatment; as in the did package, periods
  # in the anticipation window (g - anticipation <= t < g, in units of
  # tname) are not
  anticipation <- if (is.null(ptep$anticipation)) 0 else ptep$anticipation
  pre <- which(pte_calendar_time(ptep$data, group) - anticipation >
                 pte_calendar_time(ptep$data, time.period))

  # leave out cells whose variance is zero or not finite, as did::att_gt()
  # does. With a universal base period these include the reference cells,
  # which are 0 by construction with all-zero influence functions; keeping
  # them made the covariance matrix singular, so no pre-test was reported
  # (#18). With never-treated controls, the remaining universal-base
  # pre-treatment ATTs are an invertible transformation (within each group)
  # of the varying-base ones, so the Wald statistic is the same for both base
  # periods; with not-yet-treated controls the comparison groups differ, and
  # so does the statistic, slightly.
  has_variance <- is.finite(se_analytical) & se_analytical > 0
  pre <- pre[has_variance[pre]]

  # pseudo-atts in pre-treatment periods
  preatt <- as.matrix(att[pre])

  # covariance matrix of pre-treatment atts
  preV <- as.matrix(V[pre, pre])

  # check if there are actually any pre-treatment periods
  W <- NULL
  Wpval <- NULL
  if (length(preV) == 0) {
    # No pre-treatment periods available (e.g. a two-period design); W and
    # Wpval stay NULL, which already communicates this through the return
    # value without printing a message on every call.
  } else if (sum(is.na(preV))) {
    warning("Not returning pre-test Wald statistic due to NA pre-treatment values")
  } else if (rcond(preV) <= .Machine$double.eps) {
    # singluar covariance matrix for pre-treatment periods
    warning("Not returning pre-test Wald statistic due to singular covariance matrix")
  } else {
    # everything is working...
    W <- n * t(preatt) %*% solve(preV) %*% preatt
    q <- length(pre) # number of restrictions
    Wpval <- round(1 - pchisq(W, q), 5)
  }

  # convert tlist and glist to be compatible with did::aggte

  # from "new" time to "original" time
  original_time.periods <- sort(unique(ptep$data[, ptep$tname]))
  if (pte_periods_recoded(ptep, ptep$tlist)) {
    ptep$tlist <- BMisc::t2orig(ptep$tlist, original_time.periods)
    ptep$glist <- BMisc::t2orig(ptep$glist, original_time.periods)
    group <- BMisc::t2orig(group, original_time.periods)
    time.period <- BMisc::t2orig(time.period, original_time.periods)
    extra_gt_returns <- lapply(
      extra_gt_returns,
      function(egr) {
        egr$group <- BMisc::t2orig(egr$group, original_time.periods)
        egr$time.period <- BMisc::t2orig(egr$time.period, original_time.periods)
        egr
      }
    )
  }

  if (is.null(ptep$weightsname)) {
    ptep$data$.w <- 1
  } else {
    ptep$data$.w <- ptep$data[, ptep$weightsname]
  }

  # Return list for ATT(g,t)
  return(group_time_att(group = group, time.period = time.period, att = att, V_analytical = V, se = se, crit_val = cval, inf_func = inffunc, n = n, W = W, Wpval = Wpval, cband = cband, alp = alp, ptep = ptep, extra_gt_returns = extra_gt_returns))
}

#' @title Multiplier Bootstrap
#'
#' @description Function for using multiplier bootstrap to conduct
#'  inference
#'
#' @param inffunc influence function matrix
#' @param cluster cluster identifiers, one per row of \code{inffunc}, or
#'  \code{NULL} (the default) for clustering at the level of the rows (units)
#' @param cband whether to compute the critical value for uniform confidence
#'  bands (default \code{TRUE}). With \code{FALSE}, only standard errors are
#'  computed and \code{crit_val} is \code{NA}.
#' @inheritParams pte
#'
#' @return list with the following elements:
#'   - `boot_se`: bootstrap standard errors
#'   - `crit_val`: critical value for uniform confidence bands, from the
#'     sup-t statistic over the parameters with a positive standard error
#'     (the normal critical value if there is only one; \code{NA} if there
#'     are none or \code{cband = FALSE})
#'
#' @export
mboot2 <- function(inffunc, biters = 1000, alp = .05, cband = TRUE,
                   cluster = NULL) {
  if (!(is.numeric(biters) && length(biters) == 1 && !is.na(biters) &&
          biters >= 1)) {
    stop("mboot2() needs biters >= 1 bootstrap draws.", call. = FALSE)
  }
  inffunc <- as.matrix(inffunc)
  n <- nrow(inffunc)

  # Sums of the influence function within clusters; without clustering each
  # unit is its own cluster and these are the influence functions
  # themselves. With clustering, one multiplier is drawn per cluster, as in
  # did::att_gt() (Callaway and Sant'Anna 2021, Remark 10) (#6).
  inf_sums <- cluster_sums(inffunc, cluster)
  n_sums <- nrow(inf_sums)

  # run the multiplier bootstrap: draw b uses Rademacher weights U_b
  # (-1 or 1, each with p = 1/2) and gives T_b = sum_c U_bc * S_c, the sum
  # over clusters c of the multiplier times the influence-function sum S_c.
  # All draws in a block are one sample() call and one matrix product,
  # rather than an R loop over draws. One sample() call of size n * k
  # produces the same stream as k calls of size n, so the draws (and results)
  # match the draw-by-draw loop. Blocks keep the n x k weight matrix to
  # roughly 10 million entries.
  block <- max(1L, min(biters, floor(1e7 / n_sums)))
  starts <- seq.int(1L, biters, by = block)
  bres <- do.call("rbind", lapply(starts, function(s) {
    k <- min(block, biters - s + 1L)
    U <- matrix(sample(c(-1, 1), size = n_sums * k, replace = TRUE),
                nrow = n_sums, ncol = k)
    crossprod(U, inf_sums)
  }))

  # bootstrap scale of T_b (interquartile range, normalized to the SD of a
  # normal), and standard errors scale / n = (1/n) sqrt(sum_c S_c^2), the
  # cluster-robust form (with unit clusters, the usual one)
  boot_scale <- apply(
    bres, 2,
    function(b) {
      (quantile(b, .75, type = 1, na.rm = TRUE) -
        quantile(b, .25, type = 1, na.rm = TRUE)) / (qnorm(.75) - qnorm(.25))
    }
  )
  boot_se <- boot_scale / n

  # Only standard errors are needed (e.g. for a single parameter): skip the
  # sup-t statistic, which would otherwise warn on every draw for a
  # parameter with zero standard error (#3).
  if (!isTRUE(cband)) {
    return(list(boot_se = boot_se, crit_val = NA_real_))
  }

  # Critical value for uniform confidence bands from the bootstrap sup-t
  # statistic, over the parameters with a positive standard error. A
  # parameter that is 0 by construction (e.g. a universal reference period)
  # has standard error 0 and is left out (#3).
  usable <- is.finite(boot_se) & boot_se > 0
  if (!any(usable)) {
    return(list(boot_se = boot_se, crit_val = NA_real_))
  }
  if (sum(usable) == 1) {
    # with one parameter a uniform band is a pointwise interval; use the
    # normal critical value rather than a noisy bootstrap quantile of it
    return(list(boot_se = boot_se, crit_val = qnorm(1 - alp / 2)))
  }
  bT <- apply(bres[, usable, drop = FALSE], 1,
              function(b) max(abs(b / boot_scale[usable])))
  crit_val <- quantile(bT, 1 - alp, type = 1, na.rm = TRUE)
  if (crit_val < qnorm(1 - alp / 2)) {
    warning("critical value for uniform confidence band is somehow smaller than
            critical value for pointwise confidence interval...using pointwise
            confidence interal")
    crit_val <- qnorm(1 - alp / 2)
  }

  list(boot_se = boot_se, crit_val = crit_val)
}


# --- Internal: clustering (#6) ----------------------------------------------
# pte_cluster(): the cluster of each row of the influence function (units in
#   the order of unique(.id) for panel data, observations for repeated cross
#   sections), or NULL if standard errors are not clustered beyond units.
pte_cluster <- function(ptep) {
  if (is.null(ptep$clustervars) || is.null(ptep$data$.cluster)) return(NULL)
  data <- ptep$data
  if (isTRUE(ptep$panel)) {
    ids <- attr(data, "pte_ids")
    if (is.null(ids)) ids <- unique(data$.id)
    data$.cluster[match(ids, data$.id)]
  } else {
    data$.cluster
  }
}

# cluster_sums(): sums of the rows of an influence function (vector or
#   matrix) within clusters; unchanged when cluster is NULL.
cluster_sums <- function(inffunc, cluster) {
  if (is.null(cluster)) return(inffunc)
  rowsum(as.matrix(inffunc), cluster, reorder = FALSE)
}

# --- Were the time periods recoded? ------------------------------------------
#
# Internal helper. setup_pte() recodes time periods and groups to 1..T
# (.period, .G), and results are mapped back to the original periods with
# BMisc::t2orig(). setup_pte_basic() and some custom setup functions keep
# the original periods, and mapping those again would be wrong. This used
# to be guessed from whether the internal periods were all valid original
# periods, which fails whenever they happen to be: with periods 2:3 the
# internal post period 2 is also an original period, so nothing was mapped
# back and the overall ATT was NaN. When the data have .period, compare it
# with the user's time column instead; fall back to the old guess for
# setups without .period.
#
# Arguments:
#   ptep           - pte_params object
#   internal_tlist - time periods as used in the group-time loop
#
# Returns: TRUE if results must be mapped back with BMisc::t2orig()
pte_periods_recoded <- function(ptep, internal_tlist) {
  data <- ptep$data
  if (!is.null(data$.period)) {
    return(any(data$.period != data[[ptep$tname]]))
  }
  original_time.periods <- sort(unique(data[[ptep$tname]]))
  !all(internal_tlist %in% original_time.periods)
}
