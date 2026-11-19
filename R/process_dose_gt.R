# =============================================================================
# Title: Process Continuous-Treatment (Dose) Results
# Description: process_dose_gt() and dose_obj summary/print methods for
#   continuous-treatment (dose) results. Standard errors and critical values
#   for the dose case are always computed via the multiplier bootstrap;
#   analytical standard errors are not yet supported here (see dev/NOTES.md).
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2025-01-11
# =============================================================================

#' @title Process Results with a Continuous Treatment
#'
#' @description After computing results for each group and time period,
#'  `process_dose_gt` combines/averages them into overall effects and/or
#'  dose specific effects.  This is generic code that can be used
#'  from different ways of estimating causal effects across different
#'  timing groups and periods in a previous step.
#'
#' @inheritParams process_att_gt
#' @inheritParams pte_results
#'
#' @param gt_results list of group-time specific results
#' @param ... extra arguments
#'
#' @return a `dose_obj` object
#'
#' @export
process_dose_gt <- function(gt_results, ptep, ...) {
    # make the call to att, to get same format of results
    att_gt <- suppressWarnings(process_att_gt(gt_results, ptep)) # try to drop this as it does some extraneous things that we don't end up using
    o_weights <- overall_weights(att_gt, ...)

    # main dose-specific results are in extra_gt_returns
    all_extra_gt_returns <- att_gt$extra_gt_returns
    groups <- unlist(BMisc::get_list_element(all_extra_gt_returns, "group"))
    time.periods <- unlist(BMisc::get_list_element(all_extra_gt_returns, "time.period"))

    # check that order of groups and time periods matches
    if (!all(cbind(groups, time.periods) == o_weights[, c("group", "time.period")])) {
        stop("in processing dose results, mismatch between order of groups and time periods")
    }

    inner_extra_gt_returns <- BMisc::get_list_element(all_extra_gt_returns, "extra_gt_returns")

    # With a universal base period, the reference cells are 0 by construction
    # and have no dose-specific results. They are pre-treatment, so they get
    # no weight in the overall parameters; leave them out so the per-cell
    # results, weights and influence-function columns stay aligned.
    has_dose <- !vapply(inner_extra_gt_returns, is.null, logical(1))
    inner_extra_gt_returns <- inner_extra_gt_returns[has_dose]
    o_weights <- o_weights[has_dose, , drop = FALSE]
    groups <- groups[has_dose]
    time.periods <- time.periods[has_dose]
    att.d_gt <- BMisc::get_list_element(inner_extra_gt_returns, "att.d")
    acrt.d_gt <- BMisc::get_list_element(inner_extra_gt_returns, "acrt.d")
    att.overall_gt <- unlist(BMisc::get_list_element(inner_extra_gt_returns, "att.overall"))
    acrt.overall_gt <- unlist(BMisc::get_list_element(inner_extra_gt_returns, "acrt.overall"))
    bet_gt <- BMisc::get_list_element(inner_extra_gt_returns, "bet")
    bread_gt <- BMisc::get_list_element(inner_extra_gt_returns, "bread")
    Xe_gt <- BMisc::get_list_element(inner_extra_gt_returns, "Xe")

    acrt_gt_inffunc_mat <- gt_results$inffunc[, has_dose, drop = FALSE]
    # clusters for standard errors (NULL: unit level) (#6)
    cluster <- pte_cluster(ptep)
    biters <- ptep$biters
    bstrap <- ptep$bstrap
    alp <- ptep$alp

    # Without comparison units (setup_pte(require_comparison = FALSE)), only
    # the ACRT is identified: it uses variation in the dose within each
    # group, while ATT(d) and the overall ATT need untreated comparison
    # units. These are then NA (contdid #5).
    no_comparison <- isFALSE(ptep$require_comparison)

    # overall att results
    if (no_comparison) {
        overall_att <- NA_real_
        overall_att_se <- NA_real_
        overall_att_inffunc <- NULL
    } else {
        overall_att_res <- suppressWarnings(
            ptetools::pte_default(
                yname = ptep$yname,
                gname = ptep$gname,
                tname = ptep$tname,
                idname = ptep$idname,
                data = ptep$data,
                d_outcome = TRUE,
                anticipation = ptep$anticipation,
                base_period = ptep$base_period,
                control_group = ptep$control_group,
                weightsname = ptep$weightsname,
                clustervars = ptep$clustervars,
                biters = ptep$biters,
                alp = ptep$alp
            )
        )

        overall_att <- overall_att_res$overall_att$overall.att
        overall_att_se <- overall_att_res$overall_att$overall.se
        overall_att_inffunc <- overall_att_res$overall_att$inf.function[[2]]
        if (!all.equal(overall_att, weighted.mean(att.overall_gt, w = o_weights$overall_weight))) {
            stop("failed sanity check: something off with calculating overall att")
        }
    }

    # overall acrt results
    overall_acrt <- weighted.mean(acrt.overall_gt, w = o_weights$overall_weight)
    overall_acrt_inffunc <- as.matrix(rowSums(sweep(acrt_gt_inffunc_mat, 2, o_weights$overall_weight, "*")) / sum(o_weights$overall_weight))
    # with biters = 0 (no bootstrap), use the analytical standard error (#14)
    overall_acrt_se <- getSE(overall_acrt_inffunc, bstrap = biters > 0,
                             biters = biters, alp = alp, cluster = cluster)

    # point estimates of ATT(d) and ACRT(d)
    att.d <- if (no_comparison) {
        rep(NA_real_, length(ptep$dvals))
    } else {
        BMisc::weighted_combine_list(att.d_gt, o_weights$overall_weight)
    }
    acrt.d <- BMisc::weighted_combine_list(acrt.d_gt, o_weights$overall_weight)

    # values of the dose, and the spline basis (and its derivative) at them
    dvals <- ptep$dvals
    basis <- dose_spline_basis(
        dvals = dvals,
        degree = ptep$degree,
        knots = ptep$knots,
        boundary_knots = ptep$boundary_knots
    )
    bs_grid <- basis$bs_grid
    bs_deriv <- basis$bs_deriv

    # since we are picking dvals over a grid, the only randomness comes from
    # estimating the \beta's
    n1_vec <- sapply(Xe_gt, nrow)

    # each dose regression needs more treated units than parameters: with as
    # many, it fits them exactly (zero residuals, so their influence-function
    # entries are 0 and the check below fails); with fewer it is not
    # identified (#17)
    n_par_vec <- sapply(Xe_gt, ncol)
    too_small <- n1_vec <= n_par_vec
    if (any(too_small)) {
        i <- which(too_small)[1]
        stop("The dose regression for group ", groups[i], ", period ",
             time.periods[i], " has ", n1_vec[i], " treated unit(s) but ",
             n_par_vec[i], " parameters; it needs more treated units than ",
             "parameters. Use a less flexible dose specification, or drop ",
             "this group.", call. = FALSE)
    }
    n <- nrow(acrt_gt_inffunc_mat)
    # these are treated observations
    keep_mat <- acrt_gt_inffunc_mat != 0
    if (!all(colSums(keep_mat) == n1_vec)) {
        stop("Internal check failed: the number of nonzero entries of the ",
             "ACRT(d) influence function does not match the number of ",
             "treated units in some group-time cell. This can happen if some ",
             "treated units fit the dose regression exactly.", call. = FALSE)
    }

    biters <- ptep$biters
    alp <- ptep$alp
    cband <- ptep$cband
    if (no_comparison) {
        # ATT(d) is not identified without comparison units
        att.d_se <- rep(NA_real_, length(ptep$dvals))
        att.d_crit.val <- NA_real_
        att.d_inffunc <- NULL
        att_cband_ok <- TRUE
    } else {
        # comparison units in each cell: nonzero in the overall-ATT influence
        # function (same cells as above, in the same order; drop the reference
        # cells too) but not treated
        overall_gt_inffunc <-
            overall_att_res$att_gt$inffunc[, has_dose, drop = FALSE]
        keep_mat2 <- overall_gt_inffunc != 0 & !keep_mat
        comparison_inffunc <- overall_gt_inffunc
        comparison_inffunc[!keep_mat2] <- 0

        att.d_gt_inffunc <- lapply(
            1:length(Xe_gt),
            function(i) {
                out_inffunc <- matrix(data = 0, nrow = n, ncol = length(dvals))
                this_inffunc <- (Xe_gt[[i]] %*% bread_gt[[i]] %*% t(bs_grid))
                out_inffunc[keep_mat[, i], ] <- (n / n1_vec[i]) * this_inffunc
                # add comparison group part of the influence function
                # every d is the same with respect to comparison group
                # don't rescale the influence function for untreated because these
                # were already rescaled in the call to pte_default above
                out_inffunc[keep_mat2[, i], ] <- -replicate(n = ncol(out_inffunc), comparison_inffunc[keep_mat2[, i], i])
                out_inffunc
            }
        )

        att.d_inffunc <- BMisc::weighted_combine_list(att.d_gt_inffunc, o_weights$overall_weight)
        # ATT(d) and ACRT(d) standard errors come only from the multiplier
        # bootstrap; with biters = 0 they are NA (and cband is already FALSE,
        # so the critical values below are pointwise) (#14)
        if (biters == 0) {
            message("biters = 0: standard errors of ATT(d) and ACRT(d) ",
                    "require the bootstrap, so they are NA.")
            boot_res <- list(boot_se = rep(NA_real_, length(dvals)))
        } else {
            boot_res <- mboot2(att.d_inffunc, biters = biters, alp = alp,
                               cband = cband, cluster = cluster)
        }
        att.d_se <- boot_res$boot_se
        att_cband_ok <- FALSE
        if (cband) {
            att.d_crit.val <- boot_res$crit_val
            att.d_crit.val <- crit_val_checks(att.d_crit.val, alp)
            att_cband_ok <- isTRUE(attr(att.d_crit.val, "cband"))
            # strip the "cband"/quantile-name attributes now that we've read
            # them -- a length-1 value carrying extra attributes breaks
            # data.frame()/cbind.data.frame() recycling in autoplot.dose_obj()
            att.d_crit.val <- as.numeric(att.d_crit.val)
        } else {
            att.d_crit.val <- qnorm(1 - alp / 2)
        }
    }

    # influence function for acrt

    # acrt influence function - same as for att.d except use derivative of basis functions
    acrt.d_gt_inffunc <- lapply(
        1:length(Xe_gt),
        function(i) {
            out_inffunc <- matrix(data = 0, nrow = n, ncol = length(dvals))
            this_inffunc <- (Xe_gt[[i]] %*% bread_gt[[i]] %*% t(bs_deriv))
            out_inffunc[keep_mat[, i], ] <- (n / n1_vec[i]) * this_inffunc
            out_inffunc
        }
    )
    acrt.d_inffunc <- BMisc::weighted_combine_list(acrt.d_gt_inffunc, o_weights$overall_weight)
    acrt_boot_res <- if (biters == 0) {
        list(boot_se = rep(NA_real_, length(dvals)))
    } else {
        mboot2(acrt.d_inffunc, biters = biters, alp = alp, cband = cband,
               cluster = cluster)
    }
    acrt.d_se <- acrt_boot_res$boot_se
    acrt_cband_ok <- FALSE
    if (cband) {
        acrt.d_crit.val <- acrt_boot_res$crit_val
        acrt.d_crit.val <- crit_val_checks(acrt.d_crit.val, alp)
        acrt_cband_ok <- isTRUE(attr(acrt.d_crit.val, "cband"))
        acrt.d_crit.val <- as.numeric(acrt.d_crit.val)
    } else {
        acrt.d_crit.val <- qnorm(1 - alp / 2)
    }

    # If either att.d or acrt.d fell back to pointwise critical values (e.g.
    # because standard errors were NA), downgrade our own cband flag so
    # summary.dose_obj reports "Pointwise" instead of "Simult." -- the crit
    # vals themselves are already correct either way, this only affects the
    # printed label.
    if (cband && (!att_cband_ok || !acrt_cband_ok)) ptep$cband <- FALSE

    dose_obj(
        dose = dvals,
        overall_att = overall_att,
        overall_att_se = overall_att_se,
        overall_att_inffunc = overall_att_inffunc,
        overall_acrt = overall_acrt,
        overall_acrt_se = overall_acrt_se,
        overall_acrt_inffunc = overall_acrt_inffunc,
        att.d = att.d,
        att.d_se = att.d_se,
        att.d_crit.val = att.d_crit.val,
        att.d_inffunc = att.d_inffunc,
        acrt.d = acrt.d,
        acrt.d_se = acrt.d_se,
        acrt.d_crit.val = acrt.d_crit.val,
        acrt.d_inffunc = acrt.d_inffunc,
        pte_params = ptep
    )
}

# --- Internal: dose spline basis ---------------------------------------------
# B-spline basis (with an intercept column) and its derivative, evaluated at
# `dvals`. These must be the same basis the dose regressions were fitted with,
# since the influence functions of ATT(d) and ACRT(d) are linear in the
# fitted coefficients. The fitted basis has boundary knots at the range of the
# positive doses (stored by callers such as contdid in
# `ptep$boundary_knots`); bSpline() and dbs() would otherwise default to
# range(dvals), which gives a different basis whenever dvals does not span
# exactly the positive doses (ptetools #7, contdid #16). If `boundary_knots`
# is NULL, falls back to range(dvals), the behavior before ptetools 1.0.2.
#
# Arguments: dvals (doses to evaluate at), degree, knots (interior knots),
#   boundary_knots (length-2 numeric or NULL).
# Returns: list with bs_grid and bs_deriv, each length(dvals) x (1 + basis dim).
dose_spline_basis <- function(dvals, degree, knots, boundary_knots = NULL) {
    if (is.null(boundary_knots)) boundary_knots <- range(dvals)
    bs_grid <- splines2::bSpline(
        dvals,
        degree = degree, knots = knots, Boundary.knots = boundary_knots
    )
    bs_deriv <- splines2::dbs(
        dvals,
        degree = degree, knots = knots, Boundary.knots = boundary_knots
    )
    list(
        bs_grid = cbind(1, bs_grid), # add intercept
        # intercept doesn't affect the derivative; the zero column only keeps
        # dimensions aligned with bs_grid and the coefficient vector
        bs_deriv = cbind(0, bs_deriv)
    )
}

#' @title Class for Continuous Treatments
#'
#' @description Holds results from computing dose-specific treatment effects
#'  with a continuous treatment
#'
#' @param dose vector containing the values of the dose used in estimation
#' @param overall_att estimate of the overall ATT, the mean of ATT(D) given D > 0
#' @param overall_att_se the standard error of the estimate of overall_att
#' @param overall_att_inffunc the influence function for estimating overall_att
#' @param overall_acrt estimate of the overall ACRT, the mean of ACRT(D|D) given D > 0
#' @param overall_acrt_se the standard error for the estimate of overall_acrt
#' @param overall_acrt_inffunc the influence function for estimating overall_acrt
#' @param att.d estimates of ATT(d) for each value of `dose`
#' @param att.d_se standard error of ATT(d) for each value of `dose`
#' @param att.d_crit.val critical value to produce pointwise or uniform confidence
#'  interval for ATT(d)
#' @param att.d_inffunc matrix containing the influence function from estimating
#'  ATT(d)
#' @param acrt.d estimates of ACRT(d) for each value of `dose`
#' @param acrt.d_se standard error of ACRT(d) for each value of `dose`
#' @param acrt.d_crit.val critical value to produce pointwise or uniform confidence
#'  interval for ACRT(d)
#' @param acrt.d_inffunc matrix containing the influence function from estimating
#'  ACRT(d)
#' @param pte_params a pte_params object containing other parameters passed to the function
#'
#' @return a `dose_obj` object
#'
#' @export
dose_obj <- function(
    dose,
    overall_att = NULL,
    overall_att_se = NULL,
    overall_att_inffunc = NULL,
    overall_acrt = NULL,
    overall_acrt_se = NULL,
    overall_acrt_inffunc = NULL,
    att.d = NULL,
    att.d_se = NULL,
    att.d_crit.val = NULL,
    att.d_inffunc = NULL,
    acrt.d = NULL,
    acrt.d_se = NULL,
    acrt.d_crit.val = NULL,
    acrt.d_inffunc = NULL,
    pte_params = NULL) {
    out <- list(
        dose = dose,
        overall_att = overall_att,
        overall_att_se = overall_att_se,
        overall_att_inffunc = overall_att_inffunc,
        overall_acrt = overall_acrt,
        overall_acrt_se = overall_acrt_se,
        overall_acrt_inffunc = overall_acrt_inffunc,
        att.d = att.d,
        att.d_se = att.d_se,
        att.d_crit.val = att.d_crit.val,
        att.d_inffunc = att.d_inffunc,
        acrt.d = acrt.d,
        acrt.d_se = acrt.d_se,
        acrt.d_crit.val = acrt.d_crit.val,
        acrt.d_inffunc = acrt.d_inffunc,
        pte_params = pte_params
    )

    class(out) <- "dose_obj"

    out
}

#' @title summary.dose_obj
#'
#' @description summarizes a `dose_obj` object
#'
#' @param object an `dose_obj` object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return a list containing the summary of a `dose_obj` object:
#'   - `dose`: vector of dose values
#'   - `overall_att`: overall ATT estimate
#'   - `overall_att_se`: standard error of overall ATT estimate
#'   - `overall_acrt`: overall ACRT estimate
#'   - `overall_acrt_se`: standard error of overall ACRT estimate
#'   - `att.d`: vector of ATT(d) estimates
#'   - `att.d_se`: vector of standard errors for ATT(d) estimates
#'   - `att.d_crit.val`: critical value for pointwise or uniform confidence interval for ATT(d)
#'   - `acrt.d`: vector of ACRT(d) estimates
#'   - `acrt.d_se`: vector of standard errors for ACRT(d) estimates
#'   - `acrt.d_crit.val`: critical value for pointwise or uniform confidence interval for ACRT(d)
#'   - `alp`: significance level
#'   - `cband`: logical indicating whether to use simultaneous or pointwise confidence intervals
#'   - `bstrap`: logical indicating whether to use bootstrap for critical value
#'
#' @export
summary.dose_obj <- function(object, ...) {
    dose_obj <- object
    out <- list(
        dose = dose_obj$dose,
        overall_att = dose_obj$overall_att,
        overall_att_se = dose_obj$overall_att_se,
        overall_acrt = dose_obj$overall_acrt,
        overall_acrt_se = dose_obj$overall_acrt_se,
        att.d = dose_obj$att.d,
        att.d_se = dose_obj$att.d_se,
        att.d_crit.val = dose_obj$att.d_crit.val,
        acrt.d = dose_obj$acrt.d,
        acrt.d_se = dose_obj$acrt.d_se,
        acrt.d_crit.val = dose_obj$acrt.d_crit.val,
        alp = dose_obj$pte_params$alp,
        cband = dose_obj$pte_params$cband,
        bstrap = dose_obj$pte_params$bstrap
    )
    class(out) <- "summary.dose_obj"
    out
}

#' @title print.summary.dose_obj
#'
#' @description prints a summary of a `dose_obj` object
#'
#' @param x a list containing the summary of a `dose_obj` object
#' @param ... extra arguments
#'
#' @keywords internal
#' @return None. Prints a summary of a `dose_obj` object.
#' @export
print.summary.dose_obj <- function(x, ...) {
    alp <- x$alp
    cband <- x$cband
    bstrap <- x$bstrap
    if (is.null(bstrap)) bstrap <- TRUE # continuous treatment (dose) always uses the multiplier bootstrap; see dev/NOTES.md

    z <- qnorm(1 - alp / 2)
    att_cband_lower <- x$overall_att - z * x$overall_att_se
    att_cband_upper <- x$overall_att + z * x$overall_att_se
    overall_att_res <- cbind.data.frame(
        x$overall_att,
        x$overall_att_se,
        att_cband_lower,
        att_cband_upper
    )
    overall_att_res <- round(overall_att_res, 4)
    sig <- (att_cband_upper < 0) | (att_cband_lower > 0)
    sig[is.na(sig)] <- FALSE
    sig_text <- ifelse(sig, "*", "")
    overall_att_res <- cbind.data.frame(overall_att_res, sig_text)

    # print overall att
    cat("\n")
    cat("Overall ATT:  \n")
    colnames(overall_att_res) <- c("ATT", "   Std. Error", paste0("    [ ", 100 * (1 - alp), "% "), "Conf. Int.]", "")
    print(overall_att_res, row.names = FALSE)
    cat("\n")

    # overall acrt
    acrt_cband_lower <- x$overall_acrt - z * x$overall_acrt_se
    acrt_cband_upper <- x$overall_acrt + z * x$overall_acrt_se
    overall_acrt_res <- cbind.data.frame(
        x$overall_acrt,
        x$overall_acrt_se,
        acrt_cband_lower,
        acrt_cband_upper
    )
    overall_acrt_res <- round(overall_acrt_res, 4)
    sig <- (acrt_cband_upper < 0) | (acrt_cband_lower > 0)
    sig[is.na(sig)] <- FALSE
    sig_text <- ifelse(sig, "*", "")
    overall_acrt_res <- cbind.data.frame(overall_acrt_res, sig_text)

    # print overall att
    cat("\n")
    cat("Overall ACRT:  \n")
    colnames(overall_acrt_res) <- c("ACRT", "   Std. Error", paste0("    [ ", 100 * (1 - alp), "% "), "Conf. Int.]", "")
    print(overall_acrt_res, row.names = FALSE)
    cat("---\n")
    cat("Signif. codes: `*' confidence band does not cover 0")
    cat("\n\n")
}
