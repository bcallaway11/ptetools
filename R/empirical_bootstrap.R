# =============================================================================
# Title: Empirical Bootstrap for Panel Treatment Effects
# Description: panel_empirical_bootstrap and qtt_empirical_bootstrap (bootstrap
#   SE routines), plus aggregation functions attgt_pte_aggregations,
#   qtt_pte_aggregations, and qott_pte_aggregations.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2021-05-19
# =============================================================================

# --- Internal: parallel bootstrap cluster ---------------------------------
# pbapply::pblapply() runs a numeric `cl` in parallel only by forking, which
# Windows does not support, so there a numeric cl was silently ignored and
# the bootstrap ran serially (#11). On Windows, a numeric cl > 1 is turned
# into a PSOCK cluster with that many workers; its random number streams are
# seeded from the current session, so set.seed() still makes results
# reproducible. A cluster object passed as cl is used as is, on any platform.
#
# Arguments: cl (number of workers or a cluster object), os (platform; an
#   argument so the Windows path can be tested elsewhere).
# Returns: the cl to pass to pblapply(). If it is not identical to the input,
#   it is a cluster created here, and the caller must stop it.
boot_cluster <- function(cl, os = .Platform$OS.type) {
  if (is.numeric(cl) && length(cl) == 1 && cl > 1 && os == "windows") {
    cluster <- parallel::makePSOCKcluster(cl)
    parallel::clusterSetRNGStream(cluster,
                                  sample.int(.Machine$integer.max, 1))
    # attach ptetools on the workers: an attgt_fun defined in the user's
    # workspace typically calls ptetools functions (e.g. attgt_if()) without
    # the ptetools:: prefix, as it can in the main session
    parallel::clusterEvalQ(cluster, library(ptetools))
    return(cluster)
  }
  cl
}

# --- Internal: bootstrap draws without treated units ------------------------
# A bootstrap draw can contain no treated unit of any group, especially with
# small cohorts (a cohort of k units is missed with probability about
# exp(-k) per draw). Such a draw has no group-time estimates, and running
# setup and aggregation on it errored (#10). These helpers skip such draws
# and drop them, with a warning that counts them, as already happens (with
# a warning) for draws that miss only some groups.
#
# draw_has_treated(): TRUE if `bdata` contains a unit of a group in
#   ptep$glist (groups compared on the user's original scale).
draw_has_treated <- function(bdata, ptep) {
  groups <- pte_calendar_time(ptep$data, ptep$glist)
  any(bdata[[ptep$gname]] %in% groups)
}

# has_estimates(): TRUE if a list of group-time results (attgt.list from
#   compute.pte()) has at least one non-missing estimate.
has_estimates <- function(attgt.list) {
  att <- unlist(lapply(attgt.list, function(r) r$att))
  length(att) > 0 && any(!is.na(att))
}

# drop_empty_draws(): removes NULL results (draws skipped above) from the
#   list of bootstrap results, warning how many were dropped; errors if all
#   were.
drop_empty_draws <- function(boot.res) {
  empty <- vapply(boot.res, is.null, logical(1))
  if (all(empty)) {
    stop("None of the ", length(boot.res), " bootstrap draws contained ",
         "treated units, so no bootstrap standard errors can be computed.",
         call. = FALSE)
  }
  if (any(empty)) {
    warning(sum(empty), " of ", length(boot.res), " bootstrap draws had no ",
            "treated units (or no group-time estimates) and were dropped; ",
            "standard errors use the remaining ", sum(!empty), " draws.",
            call. = FALSE)
  }
  boot.res[!empty]
}

# --- Internal: bootstrap SEs by estimate --------------------------------------
# Bootstrap standard errors for estimates identified by key columns (e.g.
# group and time period, or event time), from the rows of all bootstrap
# draws. A draw can lack an estimate when it misses a small cohort (with
# probability about exp(-k) for a cohort of k units). Each standard error
# uses the draws in which its estimate exists, i.e. the bootstrap
# distribution conditional on the cohort being in the sample, as for draws
# without any treated units (#10). If an estimate is missing from more than
# 10% of the draws, its standard error is NA. Point estimates are always
# kept. Previously an estimate missing from any single draw was dropped,
# estimate and all (#22).
#
# Arguments: est (point estimates, with the key columns), draws (rows from
#   all draws, with the key columns and `value`), keys, value, n_draws
#   (number of usable draws), what (label for warnings).
# Returns: est with a column se.
boot_se_by <- function(est, draws, keys, value, n_draws, what) {
  est <- as.data.frame(est)
  draws <- as.data.frame(draws)
  key_est <- do.call(paste, unname(as.list(est[keys])))
  key_draw <- do.call(paste, unname(as.list(draws[keys])))
  by_key <- split(draws[[value]], factor(key_draw, levels = unique(key_est)))
  by_key <- lapply(by_key, function(v) v[!is.na(v)])
  n_avail <- lengths(by_key)
  enough <- n_avail >= 0.9 * n_draws & n_avail >= 2
  se <- vapply(by_key, function(v) {
    if (length(v) >= 2) stats::sd(v) else NA_real_
  }, numeric(1))
  se[!enough] <- NA_real_
  est$se <- unname(se[key_est])

  if (any(!enough)) {
    warning("Bootstrap standard errors for ", sum(!enough), " ", what,
            " are NA: they are missing from more than 10% of the ", n_draws,
            " bootstrap draws (small cohorts).", call. = FALSE)
  }
  partial <- enough & n_avail < n_draws
  if (any(partial)) {
    warning("Bootstrap standard errors for ", sum(partial), " ", what,
            " use only the draws in which they are estimated (at least ",
            min(n_avail[partial]), " of ", n_draws, "; small cohorts).",
            call. = FALSE)
  }
  est
}

# --- Internal: bootstrap resampling ----------------------------------------
# Returns a function that draws one bootstrap sample of `data`. Panel data
# are resampled by unit (block bootstrap): the rows of each unit are indexed
# once here, so each draw only stacks the sampled units' rows, instead of
# scanning all rows for every sampled unit. Sampled units get new ids
# 1, ..., n in the id column, as in BMisc::block_boot_sample(), and the same
# random draws are used: sample(uid, replace = TRUE) is uid[sample.int(...)].
# Repeated cross sections are resampled by row.
make_boot_sampler <- function(data, idname, panel, clustered = FALSE) {
  # With clustervars, whole clusters are resampled (#6). Each sampled copy of
  # a cluster gets new unit ids (panel) and row ids, so units that appear in
  # several copies are treated as distinct units.
  if (isTRUE(clustered) && !is.null(data$.cluster)) {
    clusters <- unique(data$.cluster)
    rows <- split(seq_len(nrow(data)),
                  factor(data$.cluster, levels = clusters))
    n_rows <- lengths(rows)
    return(function() {
      pos <- sample.int(length(clusters), length(clusters), replace = TRUE)
      bdata <- data[unlist(rows[pos], use.names = FALSE), , drop = FALSE]
      copy <- rep(seq_along(pos), n_rows[pos])
      if (isTRUE(panel)) {
        bdata[, idname] <- match(paste(copy, bdata[[idname]]),
                                 unique(paste(copy, bdata[[idname]])))
      }
      bdata$.rowid <- seq_len(nrow(bdata))
      bdata
    })
  }
  if (!isTRUE(panel)) {
    return(function() {
      bdata <- data[sample(seq_len(nrow(data)), replace = TRUE), ]
      bdata$.rowid <- seq_len(nrow(bdata))
      bdata
    })
  }
  uid <- unique(data[, idname])
  rows <- split(seq_len(nrow(data)), factor(data[, idname], levels = uid))
  n_rows <- lengths(rows)
  function() {
    pos <- sample.int(length(uid), length(uid), replace = TRUE)
    bdata <- data[unlist(rows[pos], use.names = FALSE), , drop = FALSE]
    bdata[, idname] <- rep(seq_along(pos), n_rows[pos])
    bdata
  }
}

#' @title Panel Empirical Bootstrap
#'
#' @description Computes empirical bootstrap pointwise standard errors
#'
#' @inheritParams compute.pte
#' @inheritParams pte
#' @inheritParams attgt_if
#' @param attgt.list list of attgt results from \code{compute.pte}
#' @param aggte_fun An optional function for aggregating group-time
#'  treatment effects.  When \code{NULL} (the default), the function is
#'  selected automatically based on \code{gt_type}.
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
panel_empirical_bootstrap <- function(attgt.list,
                                      ptep,
                                      setup_pte_fun,
                                      subset_fun,
                                      attgt_fun,
                                      extra_gt_returns,
                                      aggte_fun = NULL,
                                      ...) {
  # Resolve aggte_fun; warn if caller did not supply one (deprecated path).
  # Resolution happens before the QTT dispatch so the resolved value flows through.
  if (is.null(aggte_fun)) {
    warning(
      "aggte_fun not specified; defaulting based on gt_type. ",
      "This default will be removed in a future version of ptetools.",
      call. = FALSE
    )
    gt_type_inner <- ptep$gt_type
    aggte_fun <- switch(gt_type_inner,
      qtt  = qtt_pte_aggregations,
      qott = qott_pte_aggregations,
      function(al, p, eg) attgt_pte_aggregations(al, p)
    )
  }

  # full QTT curve mode: dispatch to dedicated bootstrap function
  if (ptep$gt_type == "qtt") {
    return(qtt_empirical_bootstrap(
      attgt.list       = attgt.list,
      ptep             = ptep,
      setup_pte_fun    = setup_pte_fun,
      subset_fun       = subset_fun,
      attgt_fun        = attgt_fun,
      extra_gt_returns = extra_gt_returns,
      aggte_fun        = aggte_fun,
      ...
    ))
  }

  # unpack ptep
  data <- ptep$data
  idname <- ptep$idname
  boot_type <- ptep$boot_type
  biters <- ptep$biters
  gt_type <- ptep$gt_type
  ret_quantile <- ptep$ret_quantile

  #-----------------------------------------------------------------------------
  # compute aggregations
  #-----------------------------------------------------------------------------

  aggte <- aggte_fun(attgt.list, ptep, extra_gt_returns)

  # kind of hack...calls and returns of emprical and multiplier bootstrap
  # not matching exactly
  original_time.periods <- sort(unique(data[, ptep$tname]))
  extra_gt_returns <- lapply(
    extra_gt_returns,
    function(egr) {
      egr$group <- BMisc::t2orig(egr$group, original_time.periods)
      egr$time.period <- BMisc::t2orig(egr$time.period, original_time.periods)
      egr
    }
  )

  # biters = 0 skips the bootstrap: point estimates only, NA standard
  # errors (#14)
  if (biters == 0) {
    message("biters = 0: standard errors require the bootstrap here, ",
            "so they are NA.")
    attgt_results <- aggte$attgt_results
    attgt_results$se <- NA_real_
    dyn_results <- aggte$dyn_results
    dyn_results$se <- NA_real_
    group_results <- aggte$group_results
    group_results$se <- NA_real_
    overall_results <- data.frame(att = aggte$overall_results,
                                  se = NA_real_, crit_val = NA_real_)
    return(pte_emp_boot(
      attgt_results = attgt_results,
      overall_results = overall_results,
      group_results = group_results,
      dyn_results = dyn_results,
      extra_gt_returns = extra_gt_returns,
      ptep = ptep
    ))
  }

  # parallel workers for the bootstrap (a PSOCK cluster on Windows, #11)
  cl <- boot_cluster(ptep$cl)
  if (!identical(cl, ptep$cl)) on.exit(parallel::stopCluster(cl), add = TRUE)

  # bootstrap
  # draws bootstrap samples; panel data are resampled by unit while repeated
  # cross sections are resampled at the observation level
  draw_sample <- make_boot_sampler(data, idname, ptep$panel,
                                   clustered = !is.null(ptep$clustervars))

  # loop for each nonparametric bootstrap iteration
  boot.res <- pbapply::pblapply(seq_len(biters), function(b) {
    bdata <- draw_sample()
    # no treated units in this draw: nothing to estimate (#10)
    if (!draw_has_treated(bdata, ptep)) return(NULL)

    bptep <- setup_pte_fun(
      yname = ptep$yname,
      gname = ptep$gname,
      tname = ptep$tname,
      idname = ptep$idname,
      data = bdata,
      panel = ptep$panel,
      alp = ptep$alp,
      boot_type = boot_type,
      gt_type = gt_type,
      # ret_quantile=ret_quantile,
      biters = ptep$biters,
      cl = ptep$cl,
      ...
    )

    # call our function for estimating attgt on the
    # bootstrapped data
    bres_gt <- compute.pte(
      ptep = bptep,
      subset_fun = subset_fun,
      attgt_fun = attgt_fun,
      ...
    )[c("attgt.list", "extra_gt_returns")] # don't need to carry around ptep
    if (!has_estimates(bres_gt$attgt.list)) return(NULL)

    bres <- aggte_fun(bres_gt$attgt.list, bptep, bres_gt$extra_gt_returns)
    bres
  }, cl = cl)
  boot.res <- drop_empty_draws(boot.res)
  # number of usable draws: event times / groups estimated in every one of
  # them get bootstrap standard errors
  n_draws <- length(boot.res)

  # standard errors of group-time, event-study and group estimates, each from
  # the draws in which it is estimated (#22)
  attgt_results <- boot_se_by(
    aggte$attgt_results,
    bind_rows(BMisc::get_list_element(boot.res, "attgt_results")),
    keys = c("group", "time.period"), value = "att", n_draws = n_draws,
    what = "group-time effects"
  )
  dyn_results <- boot_se_by(
    aggte$dyn_results,
    bind_rows(BMisc::get_list_element(boot.res, "dyn_results")),
    keys = "e", value = "att.e", n_draws = n_draws, what = "event times"
  )
  group_results <- boot_se_by(
    aggte$group_results,
    bind_rows(BMisc::get_list_element(boot.res, "group_results")),
    keys = "group", value = "att.g", n_draws = n_draws, what = "groups"
  )

  # overall results
  alp <- ptep$alp
  overall_att    <- aggte$overall_results
  overall_draws  <- unlist(BMisc::get_list_element(boot.res, "overall_results"))
  overall_se     <- sd(overall_draws)
  overall_cval   <- quantile(abs((overall_draws - overall_att) / overall_se),
                             1 - alp, type = 1)
  overall_results <- data.frame(att = overall_att, se = overall_se,
                                crit_val = overall_cval)


  # return all results
  pte_emp_boot(
    attgt_results = attgt_results,
    overall_results = overall_results,
    group_results = group_results,
    dyn_results = dyn_results,
    extra_gt_returns = extra_gt_returns,
    ptep = ptep
  )
}


#' @title Aggregate Group-Time Average Treatment Effects
#'
#' @description Aggregate group-time average treatment effects into
#'  overall, group, and dynamic effects.  This function is only used
#'  for (i) computing standard errors using the empirical bootstrap,
#'  and (ii) combining distributions at the (g,t) level
#'
#' @inheritParams panel_empirical_bootstrap
#' @inheritParams attgt_if
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
attgt_pte_aggregations <- function(attgt.list, ptep) {
  # pick up all time periods
  time.periods <- ptep$tlist
  groups <- ptep$glist

  original_time.periods <- sort(unique(ptep$data[, ptep$tname]))


  # data
  data <- ptep$data

  # turn results into a data.frame
  attgt.results <- do.call("rbind.data.frame", attgt.list)

  # this drop na att(g,t) due to violations of overlap conditions
  attgt.results <- attgt.results[complete.cases(attgt.results), ]

  # handle unequally spaced periods
  if (!(all(time.periods %in% original_time.periods))) {
    time.periods <- BMisc::t2orig(time.periods, original_time.periods)
    groups <- BMisc::t2orig(groups, original_time.periods)
    attgt.results$time.period <- BMisc::t2orig(attgt.results$time.period, original_time.periods)
    attgt.results$group <- BMisc::t2orig(attgt.results$group, original_time.periods)
  }

  # add event time to the results
  attgt.results$e <- attgt.results$time.period - attgt.results$group

  # calculate relative sizes of each group (will be used as weights):
  # weighted number of units of each group in the first period
  # Weighted by the sampling weights (.w, all 1 without weightsname), as the
  # group shares in pte_aggte() and did::aggte(); counting rows combined
  # cohorts by sample counts instead of weighted population shares (#23)
  first_rows <- which(data[, ptep$tname] == time.periods[1])
  first_G <- data[first_rows, ptep$gname]
  first_w <- data$.w[first_rows]
  if (is.null(first_w)) first_w <- rep(1, length(first_rows))
  n.group <- vapply(groups, function(gg) {
    sum(first_w[first_G == gg], na.rm = TRUE)
  }, numeric(1))
  # merge in group sizes
  ngroup.mat <- cbind(groups, n.group)
  attgt.results <- merge(attgt.results, ngroup.mat, by.x = "group", by.y = "groups")

  # event times to calculate dynamic effects
  eseq <- unique(attgt.results$e)
  eseq <- sort(eseq)

  # calculate average effects by event time
  att.e <- c()
  weights.e <- list()
  counter <- 1
  for (this.e in eseq) {
    # get subset of results at this event time
    res.e <- subset(attgt.results, e == this.e)

    # calculate weights by group size
    res.e$weight <- res.e$n.group / sum(res.e$n.group)
    weights.e[[counter]] <- list()
    weights.e[[counter]]$e <- this.e
    weights.e[[counter]]$weights <- rep(0, nrow(attgt.results)) # start w/ all 0 weights
    weights.e[[counter]]$weights[attgt.results$e == this.e] <- res.e$weight # fill in some weights

    # calculate dynamic effect as weighted average
    att.e[counter] <- sum(res.e$att * res.e$weight)

    # on to the next one
    counter <- counter + 1
  }

  # calculate average effects by group
  att.g <- data.frame(group = integer(), att.g = double(), n.group = double(), group_post_length = integer())
  weights.g <- list()
  counter <- 1
  for (this.g in groups) {
    # get subset of results at this event time
    res.g <- subset(attgt.results, group == this.g & time.period >= group)

    # calculate (g,t) weights
    weights.g[[counter]] <- list()
    weights.g[[counter]]$g <- this.g
    weights.g[[counter]]$weights <- rep(0, nrow(attgt.results)) # start w/ all 0 weights
    weights.g[[counter]]$weights[attgt.results$group == this.g & attgt.results$time.period >= attgt.results$group] <- 1 / nrow(res.g) # fill in weights

    # calculate group effect as weighted average
    att.g[counter, ] <- c(this.g, mean(res.g$att), mean(res.g$n.group), group_post_length = nrow(res.g))

    # on to the next one
    counter <- counter + 1
  }

  # drops any that are missing due to violations of overlap
  att.g <- att.g[complete.cases(att.g), ]


  # weighted average across groups to get overall att
  att.overall <- sum(att.g$att.g * (att.g$n.group / sum(att.g$n.group)))

  # att_gt weights
  # don't interpret this, this is just to put the weights back on ATT(g,t)
  att.g$g.overall.w <- (att.g$n.group / sum(att.g$n.group)) / att.g$group_post_length
  weights.overall <- dplyr::left_join(attgt.results, att.g[, c("group", "g.overall.w")], by = "group")$g.overall.w *
    (attgt.results$e >= 0)

  group.results <- att.g[, c(1, 2)] # drop group sizes

  # store dynamic effects results
  dyn.results <- cbind.data.frame(e = eseq, att.e = att.e)

  # return pte_emp_boot object
  pte_emp_boot(
    attgt_results = attgt.results[, c("group", "att", "time.period")],
    dyn_results = dyn.results,
    dyn_weights = weights.e,
    group_results = group.results,
    group_weights = weights.g,
    overall_results = att.overall,
    overall_weights = weights.overall
  )
}


# --- Internal: exact mixtures of step-function CDFs (#25) -------------------
# A weighted mixture F = sum_c w_c F_c of step-function CDFs (cell ECDFs,
# possibly weighted) is itself a step function: its jumps are at the cells'
# support points y_cj, with masses w_c * p_cj. Stacking these, sorting and
# taking cumulative sums gives F exactly at every support point, and the
# quantile at tau is the first point where F >= tau (quantile type 1).
#
# This replaces evaluating every cell on a 1,000-point grid of the pooled
# outcome (BMisc::combine_ecdfs()), which rounded each quantile up to the
# next grid point and made the QTT depend on the grid, and quantile() on the
# result, which (stats:::quantile.ecdf) rounds each jump to a multiple of
# 1/nobs and so is not exact for weighted or mixed CDFs.

# ecdf_masses(): support points and masses of a step-function CDF
#   (an "ecdf"/"stepfun" object, such as from BMisc::weighted_ecdf()).
ecdf_masses <- function(cdf) {
  if (!inherits(cdf, "stepfun")) {
    stop("QTT aggregation needs each cell's F0 and F1 to be step-function ",
         "CDFs (e.g. from BMisc::weighted_ecdf()); got an object of class ",
         class(cdf)[1], ".", call. = FALSE)
  }
  x <- stats::knots(cdf)
  list(x = x, p = diff(c(0, cdf(x))))
}

# mix_cdf(): the exact mixture of cells with masses `masses` (a list from
#   ecdf_masses()) and weights `weights`. Cells with zero weight are skipped.
#   Returns a list: x (sorted unique support points) and cdf (F at x).
mix_cdf <- function(masses, weights) {
  use <- which(!is.na(weights) & weights != 0)
  x <- unlist(lapply(masses[use], `[[`, "x"), use.names = FALSE)
  p <- unlist(Map(function(m, w) m$p * w, masses[use], weights[use]),
              use.names = FALSE)
  o <- order(x)
  x <- x[o]
  cdf <- cumsum(p[o])
  # one value per support point: F at the last of tied points
  last <- !duplicated(x, fromLast = TRUE)
  x <- x[last]
  cdf <- cdf[last]
  # normalize so F reaches 1 (aggregation weights sum to 1)
  list(x = x, cdf = cdf / cdf[length(cdf)])
}

# mix_quantile(): quantiles of a mix_cdf() result: the first support point
#   where F >= tau, as quantile(type = 1). The small tolerance keeps a
#   cumulative sum that should equal tau, but is a rounding error below it,
#   from moving the quantile one point up.
mix_quantile <- function(mix, probs) {
  idx <- findInterval(probs - 1e-12, mix$cdf, left.open = TRUE) + 1
  mix$x[pmin(idx, length(mix$x))]
}

# mix_ecdf(): a mix_cdf() result as an ecdf-class step function, for the
#   returned F0_overall / F1_overall
mix_ecdf <- function(mix) {
  BMisc::make_dist(mix$x, mix$cdf, sorted = TRUE)
}


#' @title Aggregate Group-Time Quantile Treatment Effects
#'
#' @description Aggregate group-time F0/F1 distributions into QTT curves at
#'  the overall, group, and dynamic level.  CDFs are mixed first, with the
#'  aggregation weights, and the mixtures are then inverted at all quantile
#'  levels in \code{probs}, avoiding the bias from averaging scalar QTTs.
#'  The mixtures are computed exactly, from every support point of the
#'  cells' CDFs, which must be step functions (such as those from
#'  \code{BMisc::weighted_ecdf()}).
#'
#' @inheritParams attgt_pte_aggregations
#' @inheritParams attgt_if
#'
#' @return named list with elements \code{overall_results},
#'  \code{dyn_results}, \code{group_results}, \code{F0_overall},
#'  \code{F1_overall}
#'
#' @export
qtt_pte_aggregations <- function(attgt.list, ptep, extra_gt_returns) {
  probs <- if (is.null(ptep$probs)) seq(0.05, 0.95, 0.05) else ptep$probs

  # get (g,t) aggregation weights
  attgt_res <- attgt_pte_aggregations(attgt.list, ptep = ptep)

  # support points and masses of each cell's CDFs, once
  F0_gt <- lapply(extra_gt_returns, function(egr) { # nolint: object_name_linter
    ecdf_masses(egr$extra_gt_returns$F0)
  })
  F1_gt <- lapply(extra_gt_returns, function(egr) { # nolint: object_name_linter
    ecdf_masses(egr$extra_gt_returns$F1)
  })

  # QTT at probs from the mixtures with weights w
  mix_qtt <- function(w) {
    mix_quantile(mix_cdf(F1_gt, w), probs) -
      mix_quantile(mix_cdf(F0_gt, w), probs)
  }

  # --- overall -----------------------------------------------------------
  F0_mix <- mix_cdf(F0_gt, attgt_res$overall_weights) # nolint: object_name_linter
  F1_mix <- mix_cdf(F1_gt, attgt_res$overall_weights) # nolint: object_name_linter
  overall_qtt <- mix_quantile(F1_mix, probs) - mix_quantile(F0_mix, probs)
  overall_results <- data.frame(probs = probs, qtt = overall_qtt)

  # --- dynamic -----------------------------------------------------------
  dyn_results <- do.call(rbind, lapply(attgt_res$dyn_weights, function(dw) {
    data.frame(e = dw$e, probs = probs, qtt = mix_qtt(dw$weights))
  }))

  # --- group -------------------------------------------------------------
  group_results <- do.call(rbind, lapply(attgt_res$group_weights, function(gw) {
    data.frame(group = gw$g, probs = probs, qtt = mix_qtt(gw$weights))
  }))

  list(
    overall_results = overall_results,
    dyn_results     = dyn_results,
    group_results   = group_results,
    F0_overall      = mix_ecdf(F0_mix), # nolint: object_name_linter
    F1_overall      = mix_ecdf(F1_mix)  # nolint: object_name_linter
  )
}



#' @title Empirical Bootstrap for QTT Curves
#'
#' @description Runs the empirical bootstrap for the full QTT curve case
#'  (\code{gt_type = "qtt"}).  Called automatically by
#'  \code{panel_empirical_bootstrap} when \code{gt_type == "qtt"}.
#'
#' @inheritParams panel_empirical_bootstrap
#'
#' @return \code{pte_qtt} object
#'
#' @export
qtt_empirical_bootstrap <- function(attgt.list,
                                    ptep,
                                    setup_pte_fun,
                                    subset_fun,
                                    attgt_fun,
                                    extra_gt_returns,
                                    aggte_fun = NULL,
                                    ...) {
  # When called directly (not via panel_empirical_bootstrap), resolve NULL.
  if (is.null(aggte_fun)) {
    warning(
      "aggte_fun not specified; defaulting to qtt_pte_aggregations. ",
      "This default will be removed in a future version of ptetools.",
      call. = FALSE
    )
    aggte_fun <- qtt_pte_aggregations
  }

  probs  <- if (is.null(ptep$probs)) seq(0.05, 0.95, 0.05) else ptep$probs
  data   <- ptep$data
  biters <- ptep$biters

  # point estimates
  aggte <- aggte_fun(attgt.list, ptep, extra_gt_returns)

  # biters = 0 skips the bootstrap: point estimates only, with NA standard
  # errors and confidence bounds (#14)
  if (biters == 0) {
    message("biters = 0: standard errors require the bootstrap here, ",
            "so they are NA.")
    without_se <- function(res) {
      res[c("se", "lower_pw", "upper_pw", "lower_ub", "upper_ub")] <- NA_real_
      res
    }
    return(pte_qtt( # nolint: object_usage_linter
      overall    = without_se(aggte$overall_results),
      dynamic    = without_se(aggte$dyn_results),
      group      = without_se(aggte$group_results),
      F0_overall = aggte$F0_overall, # nolint: object_name_linter
      F1_overall = aggte$F1_overall, # nolint: object_name_linter
      ptep       = ptep
    ))
  }

  # bootstrap; parallel workers as in panel_empirical_bootstrap() (#11)
  cl <- boot_cluster(ptep$cl)
  if (!identical(cl, ptep$cl)) on.exit(parallel::stopCluster(cl), add = TRUE)
  draw_sample <- make_boot_sampler(data, ptep$idname, ptep$panel,
                                   clustered = !is.null(ptep$clustervars))
  boot.res <- pbapply::pblapply(seq_len(biters), function(b) {
    bdata <- draw_sample()
    # no treated units in this draw: nothing to estimate (#10)
    if (!draw_has_treated(bdata, ptep)) return(NULL)

    bptep <- setup_pte_fun(
      yname     = ptep$yname,
      gname     = ptep$gname,
      tname     = ptep$tname,
      idname    = ptep$idname,
      data      = bdata,
      panel     = ptep$panel,
      alp       = ptep$alp,
      boot_type = ptep$boot_type,
      gt_type   = ptep$gt_type,
      probs     = ptep$probs,
      biters    = ptep$biters,
      cl        = ptep$cl,
      ...
    )

    bres_gt <- compute.pte(
      ptep       = bptep,
      subset_fun = subset_fun,
      attgt_fun  = attgt_fun,
      ...
    )[c("attgt.list", "extra_gt_returns")]
    if (!has_estimates(bres_gt$attgt.list)) return(NULL)

    bres <- aggte_fun(bres_gt$attgt.list, bptep, bres_gt$extra_gt_returns)
    # only the point estimate's overall CDFs are returned; dropping each
    # draw's (which hold every support point, #25) keeps memory from growing
    # with the sample size times biters
    bres$F0_overall <- NULL # nolint: object_name_linter
    bres$F1_overall <- NULL # nolint: object_name_linter
    bres
  }, cl = cl)
  boot.res <- drop_empty_draws(boot.res)

  alp <- ptep$alp
  z   <- qnorm(1 - alp / 2)

  # Compute uniform critical value from a bootstrap matrix and point estimates.
  # Uses IQR-based scale (more robust when some QTTs are near zero), following
  # the same approach as computeSE() in the qte package. Returns a scalar c
  # such that qtt +/- c * se gives a simultaneous band across quantiles.
  # Both pointwise and uniform CIs are always computed; cband is a plotting
  # choice, not a computation choice.
  qtt_crit_val <- function(boot_mat, qtt_est) {
    sigmahalf <- (apply(boot_mat, 2, function(b) quantile(b, 0.75, type = 1)) -
                  apply(boot_mat, 2, function(b) quantile(b, 0.25, type = 1))) /
                 (qnorm(0.75) - qnorm(0.25))
    if (any(sigmahalf == 0)) {
      sigmahalf <- pmax(apply(boot_mat, 2, sd), 1e-9)
    }
    cb <- apply(boot_mat, 1, function(q) max(abs((q - qtt_est) / sigmahalf)))
    as.numeric(quantile(cb, 1 - alp, type = 1))
  }

  # --- overall SE --------------------------------------------------------
  overall_boot_mat <- do.call(rbind, lapply(boot.res, function(br) br$overall_results$qtt))
  overall_se   <- apply(overall_boot_mat, 2, sd)
  overall_cval <- qtt_crit_val(overall_boot_mat, aggte$overall_results$qtt)
  overall_results           <- aggte$overall_results
  overall_results$se        <- overall_se
  overall_results$lower_pw  <- overall_results$qtt - z            * overall_se
  overall_results$upper_pw  <- overall_results$qtt + z            * overall_se
  overall_results$lower_ub  <- overall_results$qtt - overall_cval * overall_se
  overall_results$upper_ub  <- overall_results$qtt + overall_cval * overall_se

  # --- dynamic and group SE ------------------------------------------------
  # Each QTT curve's standard errors use the draws in which it is estimated;
  # if it is missing from more than 10% of the draws (small cohorts), its
  # standard errors and bounds are NA, but the estimates are kept (#22).
  n_draws <- length(boot.res)
  curve_se <- function(results_name, key, key_vals) {
    out <- lapply(key_vals, function(this_k) {
      boot_rows <- lapply(boot.res, function(br) {
        res <- br[[results_name]]
        vals <- res$qtt[res[[key]] == this_k]
        if (length(vals) != length(probs)) NULL else vals
      })
      n_complete <- sum(!vapply(boot_rows, is.null, logical(1)))
      if (n_complete < max(2, 0.9 * n_draws)) {
        se <- NA_real_
        this_cval <- NA_real_
      } else {
        boot_mat  <- do.call(rbind, Filter(Negate(is.null), boot_rows))
        qtt_est   <- aggte[[results_name]]$qtt[aggte[[results_name]][[key]] ==
                                                 this_k]
        se        <- unname(apply(boot_mat, 2, sd))
        this_cval <- qtt_crit_val(boot_mat, qtt_est)
      }
      df <- data.frame(probs = probs, se = se, cval = this_cval)
      df[[key]] <- this_k
      attr(df, "n_complete") <- n_complete
      df
    })
    n_complete <- vapply(out, function(df) attr(df, "n_complete"), numeric(1))
    label <- if (key == "e") "event-time" else key
    dropped <- n_complete < max(2, 0.9 * n_draws)
    if (any(dropped)) {
      warning("Bootstrap standard errors for ", sum(dropped), " ", label,
              " QTT curve(s) are NA: missing from more than 10% of the ",
              n_draws, " bootstrap draws (small cohorts).", call. = FALSE)
    }
    partial <- !dropped & n_complete < n_draws
    if (any(partial)) {
      warning("Bootstrap standard errors for ", sum(partial), " ", label,
              " QTT curve(s) use only the draws in which they are estimated ",
              "(at least ", min(n_complete[partial]), " of ", n_draws,
              "; small cohorts).", call. = FALSE)
    }
    se_df <- do.call(rbind, out)
    res <- merge(aggte[[results_name]], se_df, by = c(key, "probs"),
                 all.x = TRUE)
    res$lower_pw <- res$qtt - z        * res$se
    res$upper_pw <- res$qtt + z        * res$se
    res$lower_ub <- res$qtt - res$cval * res$se
    res$upper_ub <- res$qtt + res$cval * res$se
    res$cval     <- NULL
    res
  }
  dyn_results <- curve_se("dyn_results", "e", unique(aggte$dyn_results$e))
  group_results <- curve_se("group_results", "group",
                            unique(aggte$group_results$group))

  pte_qtt( # nolint: object_usage_linter
    overall    = overall_results,
    dynamic    = dyn_results,
    group      = group_results,
    F0_overall = aggte$F0_overall, # nolint: object_name_linter
    F1_overall = aggte$F1_overall, # nolint: object_name_linter
    ptep       = ptep
  )
}


#' @title Aggregate Group-Time Quantile of the Treatment Effect
#'
#' @description Aggregate group-time distribution of the treatment effect into
#'  overall, group, and dynamic effects, at the quantile
#'  \code{ptep$ret_quantile}. The distributions are mixed exactly, from
#'  every support point of the cells' CDFs (\code{Fte}), which must be step
#'  functions (such as those from \code{BMisc::weighted_ecdf()}).
#'
#' @inheritParams attgt_pte_aggregations
#' @inheritParams attgt_if
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
qott_pte_aggregations <- function(attgt.list, ptep, extra_gt_returns) {
  ret_quantile <- ptep$ret_quantile

  # compute results for att_gt, but we are actually just interested in getting
  # the weights here.
  attgt_res <- attgt_pte_aggregations(attgt.list, ptep = ptep)

  # support points and masses of each cell's distribution of the treatment
  # effect, once. The mixtures are exact, as for the QTT (#25); they used to
  # be evaluated on 1,000 points of [-max(y), max(y)], which rounded the
  # quantiles, missed effects below -max(y) and ran backwards when every
  # outcome was negative.
  Fte_gt <- lapply(extra_gt_returns, function(egr) { # nolint: object_name_linter
    ecdf_masses(egr$extra_gt_returns$Fte)
  })
  # quantile names as quantile() gives them ("50%"), kept for compatibility
  qname <- paste0(formatC(100 * ret_quantile, format = "fg", width = 1,
                          digits = 7), "%")
  mix_qott <- function(w) {
    stats::setNames(mix_quantile(mix_cdf(Fte_gt, w), ret_quantile), qname)
  }

  # each cell's own QOTT (exact, unlike quantile() on a weighted ECDF)
  qott_gt <- vapply(Fte_gt, function(m) {
    mix_quantile(mix_cdf(list(m), 1), ret_quantile)
  }, numeric(1))
  groups <- unlist(BMisc::get_list_element(attgt.list, "group"))
  time.periods <- unlist(BMisc::get_list_element(attgt.list, "time.period"))

  overall_qott <- mix_qott(attgt_res$overall_weights)

  dyn_qott <- lapply(attgt_res$dyn_weights, function(dw) {
    list(e = dw$e, att.e = mix_qott(dw$weights))
  })

  group_qott <- lapply(attgt_res$group_weights, function(gw) {
    list(group = gw$g, att.g = mix_qott(gw$weights))
  })

  pte_emp_boot(
    attgt_results = data.frame(
      group = groups,
      time.period = time.periods,
      att = qott_gt
    ),
    dyn_results = do.call(rbind.data.frame, dyn_qott),
    group_results = do.call(rbind.data.frame, group_qott),
    overall_results = overall_qott
  )
}
