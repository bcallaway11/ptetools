#' @title Heavy-Lifting for pte Function
#'
#' @description Function that actually computes panel treatment effects.
#'   The difference relative to \code{compute.pte} is that this function
#'   loops over time periods first (instead of groups) and tries to
#'   estimate model for untreated potential outcomes jointly for all groups.
#'
#' @inheritParams pte
#' @param ptep \code{pte_params} object
#'
#' @return a list containing the following elements:
#'   - `attgt.list`: list of ATT(g,t) estimates
#'   - `inffunc`: influence function matrix
#'   - `extra_gt_returns`: list of extra returns from gt-specific calculationsons
#'
#' @export
compute.pte <- function(ptep,
                        subset_fun,
                        attgt_fun,
                        ...) {
  #-----------------------------------------------------------------------------
  # unpack ptep
  #-----------------------------------------------------------------------------
  data <- ptep$data
  yname <- ptep$yname
  gname <- ptep$gname
  idname <- ptep$idname
  tname <- ptep$tname
  base_period <- ptep$base_period
  anticipation <- ptep$anticipation
  global_fun <- ptep$global_fun


  data <- as.data.frame(data)

  # setup data
  G <- data[, gname]
  id <- data[, idname]
  period <- data[, tname]
  n <- length(unique(data$id))

  # pick up all time periods
  time.periods <- ptep$tlist

  # sort the groups and drop the untreated group
  groups <- ptep$glist

  # list to store all group-time average treatment effects
  # that we calculate
  attgt.list <- list()
  counter <- 1
  nG <- length(groups)
  nT <- length(time.periods)
  inffunc <- matrix(data = NA, nrow = n, ncol = nG * (nT))

  # list to hold extra results from gt-specific calculations
  extra_gt_returns <- list()

  if (isTRUE(global_fun)) {
    data$y0 <- attgt_fun(data, ptep, ...)
    stop("global_fun not supported yet...")
  }

  # loop over all time periods
  for (tp in time.periods) {
    # loop over all groups
    for (g in groups) {
      if (isTRUE(base_period == "universal")) {
        if (tp == (g - 1 - anticipation)) {
          attgt.list[[counter]] <- list(
            att = 0,
            group = g,
            time.period = tp
          )

          extra_gt_returns[[counter]] <- list(
            extra_gt_returns = NULL,
            group = g,
            time.period = tp
          )
          counter <- counter + 1
          next
        }
      }

      #-----------------------------------------------------------------------------
      # code to get the right subset of the data
      #-----------------------------------------------------------------------------
      gt_subset <- subset_fun(data, g, tp, ...)
      gt_data <- gt_subset$gt_data
      n1 <- gt_subset$n1
      disidx <- gt_subset$disidx

      #-----------------------------------------------------------------------------
      # code to estimate attgt using correct relevant data
      #-----------------------------------------------------------------------------
      attgt <- attgt_fun(gt_data = gt_data, ...)

      #-----------------------------------------------------------------------------
      # process attgt results
      #   - branch based on whether or not attgt_fun returned an influence
      #     function
      #-----------------------------------------------------------------------------

      # save results
      attgt.list[[counter]] <- list(
        att = attgt$attgt,
        group = g,
        time.period = tp
      )

      extra_gt_returns[[counter]] <- list(
        extra_gt_returns = attgt$extra_gt_returns,
        group = g,
        time.period = tp
      )


      # code if influence function is available
      if (!is.null(attgt$inf_func)) {
        # adjust for relative sizes of overall data
        # and groups used for this attgt
        attgt$inf_func <- (n / n1) * attgt$inf_func

        this.inf_func <- rep(0, n)
        this.inf_func[disidx] <- attgt$inf_func
        inffunc[, counter] <- this.inf_func
      }

      # cat("counter: ", counter, "\n")
      counter <- counter + 1
      #----------------------------------------------------
    }
  }

  return(list(attgt.list = attgt.list, inffunc = inffunc, extra_gt_returns = extra_gt_returns))
}

#' @title Main Function for Generically Computing Panel Treatment Effects
#'
#' @description Main function for computing panel treatment effects
#'
#' @inheritParams pte_params
#' @param setup_pte_fun This is a function that should take in \code{data},
#'  \code{yname} (the name of the outcome variable in \code{data}),
#'  \code{gname} (the name of the group variable),
#'  \code{idname} (the name of the id variable),
#'  and possibly other arguments such as the significance level \code{alp},
#'  the number of bootstrap iterations \code{biters}, and how many clusters
#'  for parallel computing in the bootstrap \code{cl}.  The key thing that
#'  needs to be figured out in this function is which groups and time periods
#'  ATT(g,t) should be computed in.  The function should
#'  return a \code{pte_params} object which contains all of the parameters
#'  passed into the function as well as \code{glist} and \code{tlist} which
#'  should be ordered lists of groups and time periods for ATT(g,t) to be computed.
#'
#'  This function provides also provides a good place for error handling related
#'  to the types of data that can be handled.
#'
#'  The \code{pte} package contains the function \code{setup_pte} that is
#'  a lightweight function that basically just takes the data, omits
#'  the never-treated group from \code{glist} but includes all other groups
#'  and drops the first time period.  This works in cases where ATT would
#'  be identified in the 2x2 case (i.e., where there are two time periods,
#'  no units are treated in the first period and the identification strategy
#'  "works" with access to a treated and untreated group and untreated
#'  potential outcomes for both groups in the first period) --- for example,
#'  this approach works if DID is the identification strategy.
#'
#' @param subset_fun This is a function that should take in \code{data},
#'  \code{g} (for group), \code{tp} (for time period), and \code{...}
#'  and be able to return the appropriate \code{data.frame} that can be used
#'  by \code{attgt_fun} to produce ATT(g=g,t=tp).  The data frame should
#'  be constructed using \code{gt_data_frame} in order to guarantee that
#'  it has the appropriate columns that identify which group an observation
#'  belongs to, etc.
#' @param attgt_fun This is a function that should work in the case where
#'  there is a single group and the "right" number of time periods to
#'  recover an estimate of the ATT.  For example, in the contest of
#'  difference in differences, it would need to work for a single group,
#'  find the appropriate comparison group (untreated units), find the right
#'  time periods (pre- and post-treatment), and then recover an estimate
#'  of ATT for that group.  It will be called over and over separately
#'  by groups and by time periods to compute ATT(g,t)'s.
#'
#'  The function needs to work in a very specific way.  It should take in the
#'  arguments: \code{data}, \code{...}.  \code{data} should be constructed
#'  using the function \code{gt_data_frame} which checks to make sure
#'  that \code{data} has the correct columns defined.
#'  \code{...} are additional arguments (such as
#'  formulas for covariates) that \code{attgt_fun} needs.  From these arguments
#'  \code{attgt_fun} must return a list with element \code{ATT} containing the
#'  group-time average treatment effect for that group and that time period.
#'
#'  If \code{attgt_fun} returns an influence function (which should be provided
#'  in a list element named \code{inf_func}), then the code will use the
#'  multiplier bootstrap to compute standard errors for group-time average
#'  treatment effects, an overall treatment effect parameter, and a dynamic
#'  treatment effect parameter (i.e., event study parameter).  If
#'  \code{attgt_fun} does not return an influence function, then the same
#'  objects will be computed using the empirical bootstrap.  This is usually
#'  (perhaps substantially) easier to code, but also will usually be (perhaps
#'  substantially) computationally slower.
#'
#' @param boot_type should be one of "multiplier" (the default) or "empirical".
#'  The multiplier bootstrap is generally much faster, but \code{attgt_fun} needs
#'  to provide an expression for the influence function (which could be challenging
#'  to figure out).  If no influence function is provided, then the \code{pte}
#'  package will use the empirical bootstrap no matter what the value of this
#'  parameter.
#'
#' @param process_dtt_gt_fun An optional function to customize results when
#'  the gt-specific function returns the distribution of treated and untreated
#'  potential outcomes.  The default is `process_dtt_gt`, which is a function
#'  provided by the package.  See that function for an example of what this function
#'  should return.  This is unused is unused except in cases where
#'  the results involve distributions.
#'
#' @param process_dose_gt_fun An optional function to customize results when the gt-specific
#'  function returns treatment effects that depend on dose (i.e., amount of the
#'  treatment).  The default is `process_dose_gt`, which is a function provided
#'  by the package.  See that function for an example of what this function should
#'  return.  This is unused except in cases where the results involve doses.
#'
#' @param ... extra arguments that can be passed to create the correct subsets
#'  of the data (depending on \code{subset_fun}), to estimate group time
#'  average treatment effects (depending on \code{attgt_fun}), or to
#'  aggregating treatment effects (particularly useful are \code{min_e},
#'  \code{max_e}, and \code{balance_e} arguments to event study aggregations)
#'
#' @return \code{pte_results} object
#'
#' @examples
#' # example using minimum wage data
#' # and difference-in-differences identification strategy
#' library(did)
#' data(mpdta)
#' did_res <- pte(
#'   yname = "lemp",
#'   gname = "first.treat",
#'   tname = "year",
#'   idname = "countyreal",
#'   data = mpdta,
#'   setup_pte_fun = setup_pte,
#'   subset_fun = two_by_two_subset,
#'   attgt_fun = did_attgt,
#'   xformla = ~lpop
#' )
#'
#' summary(did_res)
#' ggpte(did_res)
#'
#' @export
pte <- function(yname,
                gname,
                tname,
                idname,
                data,
                setup_pte_fun,
                subset_fun,
                attgt_fun,
                cband = TRUE,
                alp = 0.05,
                boot_type = "multiplier",
                weightsname = NULL,
                gt_type = "att",
                ret_quantile = NULL,
                global_fun = FALSE,
                time_period_fun = FALSE,
                group_fun = FALSE,
                process_dtt_gt_fun = process_dtt_gt,
                process_dose_gt_fun = process_dose_gt,
                biters = 100,
                cl = 1,
                call = NULL,
                ...) {
  ptep <- setup_pte_fun(
    yname = yname,
    gname = gname,
    tname = tname,
    idname = idname,
    data = data,
    cband = cband,
    alp = alp,
    boot_type = boot_type,
    gt_type = gt_type,
    weightsname = weightsname,
    ret_quantile = ret_quantile,
    global_fun = global_fun,
    time_period_fun = time_period_fun,
    group_fun = group_fun,
    biters = biters,
    cl = cl,
    call = call,
    ...
  )

  res <- compute.pte(
    ptep = ptep,
    subset_fun = subset_fun,
    attgt_fun = attgt_fun,
    ...
  )

  # handle distributional results
  if (gt_type == "dtt") {
    stop("not supported yet...")
    return(process_dtt_gt_fun(res, ptep))
  }

  # handle aggregations into dose
  if (gt_type == "dose") {
    return(process_dose_gt_fun(res, ptep))
  }

  # otherwise, we are in the main case where the target is an ATT

  # check if no influence function provided,
  # if yes, go to alternate code for empirical
  # bootstrap
  if (all(is.na(res$inffunc)) | ptep$boot_type == "empirical") {
    return(panel_empirical_bootstrap(res$attgt.list,
      ptep,
      setup_pte_fun,
      subset_fun,
      attgt_fun,
      extra_gt_returns = res$extra_gt_returns,
      ...
    ))
  }

  att_gt <- process_att_gt(res, ptep)

  #-----------------------------------------------------------------------------
  # aggregate ATT(g,t)'s
  #-----------------------------------------------------------------------------

  # overall
  overall_att <- pte_aggte(att_gt, type = "group", bstrap = TRUE, cband = cband, alp = ptep$alp)

  # event study
  # ... for max_e and min_e
  dots <- list(...)
  min_e <- ifelse(is.null(dots$min_e), -Inf, dots$min_e)
  max_e <- ifelse(is.null(dots$max_e), Inf, dots$max_e)
  balance_e <- dots$balance_e

  event_study <- pte_aggte(att_gt, type = "dynamic", bstrap = TRUE, cband = cband, alp = ptep$alp, min_e = min_e, max_e = max_e, balance_e = balance_e)

  # output
  out <- pte_results(
    att_gt = att_gt,
    overall_att = overall_att,
    event_study = event_study,
    ptep = ptep
  )

  out
}

#' @title Default, General Function for Computing Treatment Effects with Panel Data
#'
#' @description This is a generic/example wrapper for a call to the `pte` function.
#'
#' This function provides access to difference-in-differences and unconfoundedness
#' based identification/estimation strategies given (i) panel data and (ii)
#' staggered treatment adoption
#'
#' @inheritParams pte_attgt
#' @inheritParams pte
#' @inheritParams pte_params
#'
#' @return `pte_results` object
#'
#' @examples
#' # example using minimum wage data
#' # and a lagged outcome unconfoundedness strategy
#' library(did)
#' data(mpdta)
#' lou_res <- pte_default(
#'   yname = "lemp",
#'   gname = "first.treat",
#'   tname = "year",
#'   idname = "countyreal",
#'   data = mpdta,
#'   xformula = ~lpop,
#'   d_outcome = FALSE,
#'   d_covs_formula = ~lpop,
#'   lagged_outcome_cov = TRUE
#' )
#'
#' summary(lou_res)
#' ggpte(lou_res)
#'
#' @export
pte_default <- function(yname,
                        gname,
                        tname,
                        idname,
                        data,
                        xformula = ~1,
                        d_outcome = FALSE,
                        d_covs_formula = ~ -1,
                        lagged_outcome_cov = FALSE,
                        est_method = "dr",
                        anticipation = 0,
                        base_period = "varying",
                        control_group = "notyettreated",
                        weightsname = NULL,
                        cband = TRUE,
                        alp = 0.05,
                        boot_type = "multiplier",
                        biters = 100,
                        cl = 1) {
  res <- pte(
    yname = yname,
    gname = gname,
    tname = tname,
    idname = idname,
    data = data,
    setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset,
    attgt_fun = pte_attgt,
    xformula = xformula,
    d_outcome = d_outcome,
    d_covs_formula = d_covs_formula,
    lagged_outcome_cov = lagged_outcome_cov,
    est_method = est_method,
    anticipation = anticipation,
    base_period = base_period,
    control_group = control_group,
    weightsname = weightsname,
    cband = cband,
    alp = alp,
    boot_type = boot_type,
    biters = biters,
    cl = cl
  )

  res
}
