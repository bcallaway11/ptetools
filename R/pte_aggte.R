#' @title pte_aggte
#'
#' @description This is a slight edit of the aggte function from the `did` package.
#' Currently, it only provides aggregations for "overall" treatment effects
#' and event studies.  It also will provide the weights directly which is
#' currently used for constructing aggregations based on distributions.
#' The other difference is that, `pte_aggte` provides inference results
#' where the only randomness is coming from the outcomes (not from the group
#' assignment nor from the covariates).
#'
#' @param attgt A group_time_att object to be aggregated
#' @param type The type of aggregation to be done.  Default is "overall"
pte_aggte <- function(attgt,
                      type = "overall",
                      balance_e = NULL,
                      min_e = -Inf,
                      max_e = Inf,
                      ...) {
  group <- attgt$group
  time.period <- attgt$t
  att <- attgt$att
  inf_func <- attgt$inf_func
  ptep <- attgt$ptep
  bstrap <- ptep$bstrap
  if (is.null(bstrap)) bstrap <- TRUE # default to bootstrap
  cband <- ptep$cband
  alp <- ptep$alp
  biters <- ptep$biters
  data <- ptep$data
  tname <- ptep$tname
  gname <- ptep$gname
  glist <- sort(unique(group))
  tlist <- sort(unique(time.period))

  first_period_data <- data[data[, tname] == tlist[1], ]

  originalt <- time.period
  originalgroup <- group
  originalglist <- glist
  originaltlist <- tlist

  # In case g's are not part of tlist
  # originalgtlist <- sort(unique(c(originaltlist, originalglist)))
  # uniquet <- seq(1, length(unique(originalgtlist)))

  time.period <- sapply(originalt, orig2t, originaltlist)
  group <- sapply(originalgroup, orig2t, originaltlist)
  glist <- sapply(originalglist, orig2t, originaltlist)
  tlist <- unique(time.period)
  maxT <- max(time.period)

  weights.ind <- first_period_data$.w

  # relative group sizes for all ever-treated groups
  pg <- sapply(originalglist, function(g) mean(weights.ind * (first_period_data[, gname] == g)))
  # normalized so that probabilities sum to 1
  pg <- pg / sum(pg)

  # length of this is equal to number of groups
  pgg <- pg

  # same but length is equal to the number of ATT(g,t)
  pg <- pg[match(group, glist)]

  # which group time average treatment effects are post-treatment
  keepers <- which(group <= time.period & time.period <= (group + max_e)) ### added second condition to allow for limit on longest period included in att

  # n x 1 vector of group variable
  G <- unlist(lapply(first_period_data[, gname], orig2t, originaltlist))

  if (type == "group") {
    # get group specific ATTs
    # note: there are no estimated weights here
    selective.att.g <- sapply(glist, function(g) {
      # look at post-treatment periods for group g
      whichg <- which((group == g) & (g <= time.period) & (time.period <= (group + max_e))) ### added last condition to allow for limit on longest period included in att
      attg <- att[whichg]
      mean(attg)
    })
    selective.att.g[is.nan(selective.att.g)] <- NA


    # get standard errors for each group specific ATT
    selective.se.inner <- lapply(glist, function(g) {
      whichg <- which((group == g) & (g <= time.period) & (time.period <= (group + max_e))) ### added last condition to allow for limit on longest period included in att
      inf.func.g <- as.numeric(get_agg_inf_func(
        att = att,
        inffunc1 = inf_func,
        whichones = whichg,
        weights.agg = pg[whichg] / sum(pg[whichg]),
        wif = NULL
      ))
      se.g <- getSE(as.matrix(inf.func.g), bstrap = TRUE, biters = biters, alp = alp)
      list(inf.func = inf.func.g, se = se.g)
    })

    # recover standard errors separately by group
    selective.se.g <- unlist(BMisc::getListElement(selective.se.inner, "se"))
    selective.se.g[selective.se.g <= sqrt(.Machine$double.eps) * 10] <- NA

    # recover influence function separately by group
    selective.inf.func.g <- simplify2array(BMisc::getListElement(selective.se.inner, "inf.func"))

    # use multiplier bootstrap (across groups) to get critical value
    # for constructing uniform confidence bands
    selective.crit.val <- stats::qnorm(1 - alp / 2)
    if (cband == TRUE) {
      if (bstrap == FALSE) {
        warning("Used bootstrap procedure to compute simultaneous confidence band")
      }
      selective.crit.val <- mboot2(selective.inf.func.g, biters = biters, alp = alp)$crit_val

      if (is.na(selective.crit.val) | is.infinite(selective.crit.val)) {
        warning("Simultaneous critival value is NA. This probably happened because we cannot compute t-statistic (std errors are NA). We then report pointwise conf. intervals.")
        selective.crit.val <- stats::qnorm(1 - alp / 2)
        dp$cband <- FALSE
      }

      if (selective.crit.val < stats::qnorm(1 - alp / 2)) {
        warning("Simultaneous conf. band is somehow smaller than pointwise one using normal approximation. Since this is unusual, we are reporting pointwise confidence intervals")
        selective.crit.val <- stats::qnorm(1 - alp / 2)
        dp$cband <- FALSE
      }

      if (selective.crit.val >= 7) {
        warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
      }
    }

    # get overall att under selective treatment timing
    # (here use pgg instead of pg because we can just look at each group)
    selective.att <- sum(selective.att.g * pgg) / sum(pgg)

    # account for having to estimate pgg in the influence function
    selective.wif <- wif(
      keepers = 1:length(glist),
      pg = pgg,
      weights.ind = weights.ind,
      G = G,
      group = group
    )

    # get overall influence function
    selective.inf.func <- get_agg_inf_func(
      att = selective.att.g,
      inffunc1 = selective.inf.func.g,
      whichones = (1:length(glist)),
      weights.agg = pgg / sum(pgg),
      wif = selective.wif
    )


    selective.inf.func <- as.matrix(selective.inf.func)
    # get overall standard error
    selective.se <- getSE(selective.inf.func, bstrap = bstrap, biters = biters, alp = alp)
    if (!is.na(selective.se)) {
      if ((selective.se <= sqrt(.Machine$double.eps) * 10)) selective.se <- NA
    }

    return(AGGTEobj(
      overall.att = selective.att,
      overall.se = selective.se,
      type = type,
      egt = originalglist,
      att.egt = selective.att.g,
      se.egt = selective.se.g,
      crit.val.egt = selective.crit.val,
      inf.function = list(
        selective.inf.func.g = selective.inf.func.g,
        selective.inf.func = selective.inf.func
      ),
      call = call,
      DIDparams = attgt
    ))
  } else if (type == "dynamic") {
    # event times
    # this looks at all available event times
    # note: event times can be negative here.
    # note: event time = 0 corresponds to "on impact"
    # eseq <- unique(t-group)
    eseq <- unique(originalt - originalgroup)
    eseq <- eseq[order(eseq)]

    # if the user specifies balance_e, then we are going to
    # drop some event times and some groups; if not, we just
    # keep everything (that is what this variable is for)
    include.balanced.gt <- rep(TRUE, length(originalgroup))

    # if we balance the sample with resepect to event time
    if (!is.null(balance_e)) {
      include.balanced.gt <- (t2orig(maxT, originaltlist) - originalgroup >= balance_e)

      eseq <- unique(originalt[include.balanced.gt] - originalgroup[include.balanced.gt])
      eseq <- eseq[order(eseq)]

      eseq <- eseq[(eseq <= balance_e) & (eseq >= balance_e - t2orig(maxT, originaltlist) + t2orig(1, originaltlist))]
    }

    # only looks at some event times
    eseq <- eseq[(eseq >= min_e) & (eseq <= max_e)]

    # compute atts that are specific to each event time
    dynamic.att.e <- sapply(eseq, function(e) {
      # keep att(g,t) for the right g&t as well as ones that
      # are not trimmed out from balancing the sample
      whiche <- which((originalt - originalgroup == e) & (include.balanced.gt))
      atte <- att[whiche]
      pge <- pg[whiche] / (sum(pg[whiche]))
      sum(atte * pge)
    })

    # compute standard errors for dynamic effects
    dynamic.se.inner <- lapply(eseq, function(e) {
      whiche <- which((originalt - originalgroup == e) & (include.balanced.gt))
      pge <- pg[whiche] / (sum(pg[whiche]))
      wif.e <- wif(whiche, pg, weights.ind, G, group)
      inf.func.e <- as.numeric(get_agg_inf_func(
        att = att,
        inffunc1 = inf_func,
        whichones = whiche,
        weights.agg = pge,
        wif = wif.e
      ))
      se.e <- getSE(as.matrix(inf.func.e), bstrap = bstrap, biters = biters, alp = alp)
      list(inf.func = inf.func.e, se = se.e)
    })

    dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
    dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps) * 10] <- NA

    dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))

    dynamic.crit.val <- stats::qnorm(1 - alp / 2)
    if (cband == TRUE) {
      if (bstrap == FALSE) {
        warning("Used bootstrap procedure to compute simultaneous confidence band")
      }
      dynamic.crit.val <- mboot2(dynamic.inf.func.e, biters = biters, alp = alp)$crit_val

      if (is.na(dynamic.crit.val) | is.infinite(dynamic.crit.val)) {
        warning("Simultaneous critival value is NA. This probably happened because we cannot compute t-statistic (std errors are NA). We then report pointwise conf. intervals.")
        dynamic.crit.val <- stats::qnorm(1 - alp / 2)
        dp$cband <- FALSE
      }

      if (dynamic.crit.val < stats::qnorm(1 - alp / 2)) {
        warning("Simultaneous conf. band is somehow smaller than pointwise one using normal approximation. Since this is unusual, we are reporting pointwise confidence intervals")
        dynamic.crit.val <- stats::qnorm(1 - alp / 2)
        dp$cband <- FALSE
      }

      if (dynamic.crit.val >= 7) {
        warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
      }
    }

    # get overall average treatment effect
    # by averaging over positive dynamics
    epos <- eseq >= 0
    dynamic.att <- mean(dynamic.att.e[epos])
    dynamic.inf.func <- get_agg_inf_func(
      att = dynamic.att.e[epos],
      inffunc1 = as.matrix(dynamic.inf.func.e[, epos]),
      whichones = (1:sum(epos)),
      weights.agg = (rep(1 / sum(epos), sum(epos))),
      wif = NULL
    )

    dynamic.se <- getSE(dynamic.inf.func, bstrap = bstrap, biters = biters, alp = alp)
    if (!is.na(dynamic.se)) {
      if (dynamic.se <= sqrt(.Machine$double.eps) * 10) dynamic.se <- NA
    }

    return(AGGTEobj(
      overall.att = dynamic.att,
      overall.se = dynamic.se,
      type = type,
      egt = eseq,
      att.egt = dynamic.att.e,
      se.egt = dynamic.se.e,
      crit.val.egt = dynamic.crit.val,
      inf.function = list(
        dynamic.inf.func.e = dynamic.inf.func.e,
        dynamic.inf.func = dynamic.inf.func
      ),
      call = call,
      min_e = min_e,
      max_e = max_e,
      balance_e = balance_e,
      DIDparams = attgt
    ))
  }
}

#' @title Compute extra term in influence function due to estimating weights
#'
#' @description A function to compute the extra term that shows up in the
#'  influence function for aggregated treatment effect parameters
#'  due to estimating the weights
#'
#' @param keepers a vector of indices for which group-time average
#'  treatment effects are used to compute a particular aggregated parameter
#' @param pg a vector with same length as total number of group-time average
#'  treatment effects that contains the probability of being in particular group
#' @param weights.ind additional sampling weights (nx1)
#' @param G vector containing which group a unit belongs to (nx1)
#' @param group vector of groups
#'
#' @return nxk influence function matrix
#'
#' @keywords internal
wif <- function(keepers, pg, weights.ind, G, group) {
  # note: weights are all of the form P(G=g|cond)/sum_cond(P(G=g|cond))
  # this is equal to P(G=g)/sum_cond(P(G=g)) which simplifies things here

  # effect of estimating weights in the numerator
  if1 <- sapply(keepers, function(k) {
    (weights.ind * 1 * BMisc::TorF(G == group[k]) - pg[k]) /
      sum(pg[keepers])
  })
  # effect of estimating weights in the denominator
  if2 <- base::rowSums(sapply(keepers, function(k) {
    weights.ind * 1 * BMisc::TorF(G == group[k]) - pg[k]
  })) %*%
    t(pg[keepers] / (sum(pg[keepers])^2))

  # return the influence function for the weights
  if1 - if2
}


#' @title Get an influence function for particular aggregate parameters
#'
#' @title This is a generic internal function for combining influence
#'  functions across ATT(g,t)'s to return an influence function for
#'  various aggregated treatment effect parameters.
#'
#' @param att vector of group-time average treatment effects
#' @param inffunc1 influence function for all group-time average treatment effects
#'  (matrix)
#' @param whichones which elements of att will be used to compute the aggregated
#'  treatment effect parameter
#' @param weights.agg the weights to apply to each element of att(whichones);
#'  should have the same dimension as att(whichones)
#' @param wif extra influence function term coming from estimating the weights;
#'  should be n x k matrix where k is dimension of whichones
#'
#' @return nx1 influence function
#'
#' @keywords internal
get_agg_inf_func <- function(att, inffunc1, whichones, weights.agg, wif = NULL) {
  # enforce weights are in matrix form
  weights.agg <- as.matrix(weights.agg)

  # multiplies influence function times weights and sums to get vector of weighted IF (of length n)
  thisinffunc <- inffunc1[, whichones] %*% weights.agg

  # Incorporate influence function of the weights
  if (!is.null(wif)) {
    thisinffunc <- thisinffunc + wif %*% as.matrix(att[whichones])
  }

  # return influence function
  return(thisinffunc)
}


#' @title Take influence function and return standard errors
#'
#' @description Function to take an nx1 influence function and return
#'  a standard error
#'
#' @param thisinffunc An influence function
#' @inheritParams pte_aggte
#'
#' @return scalar standard error
#'
#' @keywords internal
getSE <- function(thisinffunc, bstrap = TRUE, biters = 100, alp = .05) {
  n <- length(thisinffunc)
  if (bstrap) {
    bout <- mboot2(thisinffunc, biters = biters, alp = alp)
    return(bout$boot_se)
  } else {
    return(sqrt(mean((thisinffunc)^2) / n))
  }
}


#' @title overall_weights
#'
#' @description A function that returns weights on gt's to deliver overall
#'  (averaged across groups and time periods) treatment effect parameters
#' @inheritParams pte_aggte
#' @return a data.frame with columns containing the group, the time period
#'  and the amount of weight that it should get for an overall treatment
#'  effect parameter
#' @export
overall_weights <- function(attgt,
                            balance_e = NULL,
                            min_e = -Inf,
                            max_e = Inf,
                            ...) {
  browser()
  group <- attgt$group
  time.period <- attgt$t
  att <- attgt$att
  inf_func <- attgt$inf_func
  ptep <- attgt$ptep
  bstrap <- ptep$bstrap
  if (is.null(bstrap)) bstrap <- TRUE # default to bootstrap
  cband <- ptep$cband
  alp <- ptep$alp
  biters <- ptep$biters
  data <- ptep$data
  tname <- ptep$tname
  gname <- ptep$gname
  glist <- sort(unique(group))
  tlist <- sort(unique(time.period))

  first_period_data <- data[data[, tname] == tlist[1], ]

  originalt <- time.period
  originalgroup <- group
  originalglist <- glist
  originaltlist <- tlist

  # In case g's are not part of tlist
  # originalgtlist <- sort(unique(c(originaltlist, originalglist)))
  # uniquet <- seq(1, length(unique(originalgtlist)))

  time.period <- sapply(originalt, orig2t, originaltlist)
  group <- sapply(originalgroup, orig2t, originaltlist)
  glist <- sapply(originalglist, orig2t, originaltlist)
  tlist <- unique(time.period)
  maxT <- max(time.period)

  weights.ind <- first_period_data$.w

  # relative group sizes for all ever-treated groups
  pg <- sapply(originalglist, function(g) mean(weights.ind * (first_period_data[, gname] == g)))
  # normalized so that probabilities sum to 1
  pg <- pg / sum(pg)

  # length of this is equal to number of groups
  pgg <- pg

  # same but length is equal to the number of ATT(g,t)
  pg <- pg[match(group, glist)]

  # which g-t's should get positive weight, mainly post-treatment ones
  keeper <- group <= time.period & time.period <= (group + max_e)

  g_weight <- sapply(glist, function(g) {
    # look at post-treatment periods for group g
    # added last condition to allow for limit on longest period included in att
    is_this_g <- (group == g) & (g <= time.period) & (time.period <= (group + max_e))

    # probability for this group (there is only one, so just take the first one)
    this_pg <- pg[is_this_g][1]

    # scale probability by number of post-treatment periods for this group
    this_pg / sum(is_this_g)
  })

  out_weight <- g_weight[match(group, glist)] * keeper

  # quick sanity check
  if (sum(out_weight) != 1) stop("something's going wrong calculating overall weights")

  data.frame(group = group, time.period = time.period, overall_weight = out_weight)
}
