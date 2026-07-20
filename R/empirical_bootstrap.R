# =============================================================================
# Title: Empirical Bootstrap for Panel Treatment Effects
# Description: panel_empirical_bootstrap and qtt_empirical_bootstrap (bootstrap
#   SE routines), plus aggregation functions attgt_pte_aggregations,
#   qtt_pte_aggregations, and qott_pte_aggregations.
# Author: Brant Callaway
# Last update: 2026-05-18
# Date created: 2021-05-19
# =============================================================================

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
  cl <- ptep$cl
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

  # bootstrap
  # list to store bootstrap results
  boot.res <- list()

  # loop for each nonparametric bootstrap iteration
  boot.res <- pbapply::pblapply(1:biters, function(b) {
    # draw a bootstrap sample; panel data are resampled by unit while repeated
    # cross sections are resampled at the observation level
    if (isTRUE(ptep$panel)) {
      bdata <- BMisc::blockBootSample(data, idname)
    } else {
      bdata <- data[sample(seq_len(nrow(data)), replace = TRUE), ]
      bdata$.rowid <- seq_len(nrow(bdata))
      bdata$id <- bdata$.rowid
    }

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
    # bptep <- ptep
    # bptep$data <- bdata

    # call our function for estimating attgt on the
    # bootstrapped data
    bres_gt <- compute.pte(
      ptep = bptep,
      subset_fun = subset_fun,
      attgt_fun = attgt_fun,
      ...
    )[c("attgt.list", "extra_gt_returns")] # don't need to carry around ptep

    bres <- aggte_fun(bres_gt$attgt.list, bptep, bres_gt$extra_gt_returns)
    bres
  }, cl = cl)

  # attgt results
  attgt_results_inner <- bind_rows(BMisc::get_list_element(boot.res, "attgt_results")) |>
    group_by(group, time.period)
  attgt_results_se <- unlist(attgt_results_inner |>
    group_map(~ sd(.x$att)))
  attgt_results <- aggte$attgt_results
  attgt_results$se <- attgt_results_se

  # dynamic results
  dyn_results_inner <- bind_rows(BMisc::get_list_element(boot.res, "dyn_results")) |>
    group_by(e) |>
    mutate(length.e = length(e))
  original_elength <- length(unique(dyn_results_inner$e))
  dyn_results_inner <- subset(dyn_results_inner, length.e == biters)
  new_elength <- length(unique(dyn_results_inner$e))
  if (new_elength != original_elength) {
    warning("dropping some event times due to small groups")
  }

  dyn_results_se <- dyn_results_inner |>
    transmute(se = sd(att.e))
  dyn_results_se <- dyn_results_se[1:new_elength, ]

  dyn_results <- aggte$dyn_results
  dyn_results <- inner_join(dyn_results, dyn_results_se, by = "e")

  # group results
  group_results <- aggte$group_results
  group_results_inner <- bind_rows(BMisc::get_list_element(boot.res, "group_results")) |>
    group_by(group) |>
    mutate(length.group = length(group))
  original_glength <- length(unique(group_results_inner$group))
  group_results_inner <- subset(group_results_inner, length.group == biters)
  new_glength <- length(unique(group_results_inner$group))
  if (new_glength != original_glength) {
    warning("dropping some groups due to small groups")
  }

  group_results_se <- group_results_inner |>
    transmute(se = sd(att.g))
  group_results_se <- group_results_se[1:new_glength, ]

  group_results <- inner_join(group_results, group_results_se, by = "group")

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
    time.periods <- sapply(
      time.periods,
      BMisc::t2orig,
      original_time.periods
    )
    groups <- sapply(
      groups,
      BMisc::t2orig,
      original_time.periods
    )

    attgt.results$time.period <- sapply(
      attgt.results$time.period,
      BMisc::t2orig,
      original_time.periods
    )
    attgt.results$group <- sapply(
      attgt.results$group,
      BMisc::t2orig,
      original_time.periods
    )
  }

  # add event time to the results
  attgt.results$e <- attgt.results$time.period - attgt.results$group

  # calculate relative sizes of each group
  # (will be used as weights)
  n.group <- sapply(groups, function(gg) {
    nrow(subset(data, data[, ptep$gname] == gg & data[, ptep$tname] == time.periods[1]))
  })
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
  att.g <- data.frame(group = integer(), att.g = double(), n.group = integer(), group_post_length = integer())
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


#' @title Aggregate Group-Time Quantile Treatment Effects
#'
#' @description Aggregate group-time F0/F1 distributions into QTT curves at
#'  the overall, group, and dynamic level.  CDFs are mixed first using
#'  \code{BMisc::combine_ecdfs} and then inverted at all quantile levels in
#'  \code{probs}, avoiding the bias from averaging scalar QTTs.
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

  F0_gt <- lapply(extra_gt_returns, function(egr) egr$extra_gt_returns$F0) # nolint: object_name_linter
  F1_gt <- lapply(extra_gt_returns, function(egr) egr$extra_gt_returns$F1) # nolint: object_name_linter

  yname <- ptep$yname
  y.seq <- quantile(ptep$data[, yname], probs = seq(0, 1, length.out = 1000))

  # --- overall -----------------------------------------------------------
  F0_overall <- BMisc::combine_ecdfs( # nolint: object_name_linter
    y.seq = y.seq, ecdflist = F0_gt, weights = attgt_res$overall_weights
  )
  F1_overall <- BMisc::combine_ecdfs( # nolint: object_name_linter
    y.seq = y.seq, ecdflist = F1_gt, weights = attgt_res$overall_weights
  )
  overall_qtt <- quantile(F1_overall, probs = probs, type = 1) -
    quantile(F0_overall, probs = probs, type = 1)
  overall_results <- data.frame(probs = probs, qtt = overall_qtt)

  # --- dynamic -----------------------------------------------------------
  dyn_results_list <- lapply(attgt_res$dyn_weights, function(dw) {
    F0_e <- BMisc::combine_ecdfs( # nolint: object_name_linter
      y.seq = y.seq, ecdflist = F0_gt, weights = dw$weights
    )
    F1_e <- BMisc::combine_ecdfs( # nolint: object_name_linter
      y.seq = y.seq, ecdflist = F1_gt, weights = dw$weights
    )
    qtt_e <- quantile(F1_e, probs = probs, type = 1) -
      quantile(F0_e, probs = probs, type = 1)
    data.frame(e = dw$e, probs = probs, qtt = qtt_e)
  })
  dyn_results <- do.call(rbind, dyn_results_list)

  # --- group -------------------------------------------------------------
  group_results_list <- lapply(attgt_res$group_weights, function(gw) {
    F0_g <- BMisc::combine_ecdfs( # nolint: object_name_linter
      y.seq = y.seq, ecdflist = F0_gt, weights = gw$weights
    )
    F1_g <- BMisc::combine_ecdfs( # nolint: object_name_linter
      y.seq = y.seq, ecdflist = F1_gt, weights = gw$weights
    )
    qtt_g <- quantile(F1_g, probs = probs, type = 1) -
      quantile(F0_g, probs = probs, type = 1)
    data.frame(group = gw$g, probs = probs, qtt = qtt_g)
  })
  group_results <- do.call(rbind, group_results_list)

  list(
    overall_results = overall_results,
    dyn_results     = dyn_results,
    group_results   = group_results,
    F0_overall      = F0_overall, # nolint: object_name_linter
    F1_overall      = F1_overall  # nolint: object_name_linter
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

  # bootstrap
  boot.res <- pbapply::pblapply(seq_len(biters), function(b) {
    if (isTRUE(ptep$panel)) {
      bdata <- BMisc::blockBootSample(data, ptep$idname)
    } else {
      bdata <- data[sample(seq_len(nrow(data)), replace = TRUE), ]
      bdata$.rowid <- seq_len(nrow(bdata))
      bdata$id     <- bdata$.rowid
    }

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

    aggte_fun(bres_gt$attgt.list, bptep, bres_gt$extra_gt_returns)
  }, cl = ptep$cl)

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

  # --- dynamic SE --------------------------------------------------------
  dyn_e_vals <- unique(aggte$dyn_results$e)
  dyn_se_list <- lapply(dyn_e_vals, function(this_e) {
    boot_rows <- lapply(boot.res, function(br) {
      vals <- br$dyn_results$qtt[br$dyn_results$e == this_e]
      if (length(vals) != length(probs)) NULL else vals
    })
    n_complete <- sum(!sapply(boot_rows, is.null))
    if (n_complete < 2) {
      warning(paste0("dropping event time ", this_e, " from dynamic QTT (small groups)"))
      return(NULL)
    }
    boot_mat  <- do.call(rbind, Filter(Negate(is.null), boot_rows))
    qtt_est   <- aggte$dyn_results$qtt[aggte$dyn_results$e == this_e]
    this_cval <- qtt_crit_val(boot_mat, qtt_est)
    data.frame(e = this_e, probs = probs, se = unname(apply(boot_mat, 2, sd)),
               cval = this_cval)
  })
  dyn_se_df   <- do.call(rbind, Filter(Negate(is.null), dyn_se_list))
  dyn_results <- merge(aggte$dyn_results, dyn_se_df, by = c("e", "probs"), all.x = FALSE)
  dyn_results$lower_pw <- dyn_results$qtt - z                  * dyn_results$se
  dyn_results$upper_pw <- dyn_results$qtt + z                  * dyn_results$se
  dyn_results$lower_ub <- dyn_results$qtt - dyn_results$cval   * dyn_results$se
  dyn_results$upper_ub <- dyn_results$qtt + dyn_results$cval   * dyn_results$se
  dyn_results$cval     <- NULL

  # --- group SE ----------------------------------------------------------
  group_vals <- unique(aggte$group_results$group)
  group_se_list <- lapply(group_vals, function(this_g) {
    boot_rows <- lapply(boot.res, function(br) {
      vals <- br$group_results$qtt[br$group_results$group == this_g]
      if (length(vals) != length(probs)) NULL else vals
    })
    n_complete <- sum(!sapply(boot_rows, is.null))
    if (n_complete < 2) {
      warning(paste0("dropping group ", this_g, " from group QTT (small groups)"))
      return(NULL)
    }
    boot_mat  <- do.call(rbind, Filter(Negate(is.null), boot_rows))
    qtt_est   <- aggte$group_results$qtt[aggte$group_results$group == this_g]
    this_cval <- qtt_crit_val(boot_mat, qtt_est)
    data.frame(group = this_g, probs = probs, se = unname(apply(boot_mat, 2, sd)),
               cval = this_cval)
  })
  group_se_df   <- do.call(rbind, Filter(Negate(is.null), group_se_list))
  group_results <- merge(aggte$group_results, group_se_df, by = c("group", "probs"), all.x = FALSE)
  group_results$lower_pw <- group_results$qtt - z                    * group_results$se
  group_results$upper_pw <- group_results$qtt + z                    * group_results$se
  group_results$lower_ub <- group_results$qtt - group_results$cval   * group_results$se
  group_results$upper_ub <- group_results$qtt + group_results$cval   * group_results$se
  group_results$cval     <- NULL

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
#'  overall, group, and dynamic effects.
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
  Fte_gt <- lapply(extra_gt_returns, function(egr) egr$extra_gt_returns$Fte)
  qott_gt <- unlist(lapply(1:length(Fte_gt), function(j) {
    quantile(Fte_gt[[j]], probs = ret_quantile, type = 1)
  }))
  groups <- unlist(BMisc::get_list_element(attgt.list, "group"))
  time.periods <- unlist(BMisc::get_list_element(attgt.list, "time.period"))
  yname <- ptep$yname
  y.seq <- seq(-max(ptep$data[, yname]), max(ptep$data[, yname]), length.out = 1000)

  Fte_overall <- BMisc::combine_ecdfs(
    y.seq = y.seq,
    ecdflist = Fte_gt,
    weights = attgt_res$overall_weights
  )
  overall_qott <- quantile(Fte_overall, probs = ret_quantile, type = 1)

  dyn_qott <- lapply(attgt_res$dyn_weights, function(dw) {
    Fte_e <- BMisc::combine_ecdfs(
      y.seq = y.seq,
      ecdflist = Fte_gt,
      weights = dw$weights
    )
    list(e = dw$e, att.e = quantile(Fte_e, probs = ret_quantile, type = 1))
  })

  group_qott <- lapply(attgt_res$group_weights, function(gw) {
    Fte_g <- BMisc::combine_ecdfs(
      y.seq = y.seq,
      ecdflist = Fte_gt,
      weights = gw$weights
    )
    list(group = gw$g, att.g = quantile(Fte_g, probs = ret_quantile, type = 1))
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
