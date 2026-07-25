# =============================================================================
# Title: Process Continuous-Treatment (Dose) Results
# Description: process_dose_gt() and dose_obj summary/print methods for
#   continuous-treatment (dose) results. Standard errors and critical values
#   for the dose case are always computed via the multiplier bootstrap;
#   analytical standard errors are not yet supported here (see dev/NOTES.md).
# Author: Brant Callaway
# Last update: 2026-07-25
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
    att.d_gt <- BMisc::get_list_element(inner_extra_gt_returns, "att.d")
    acrt.d_gt <- BMisc::get_list_element(inner_extra_gt_returns, "acrt.d")
    att.overall_gt <- unlist(BMisc::get_list_element(inner_extra_gt_returns, "att.overall"))
    acrt.overall_gt <- unlist(BMisc::get_list_element(inner_extra_gt_returns, "acrt.overall"))
    bet_gt <- BMisc::get_list_element(inner_extra_gt_returns, "bet")
    bread_gt <- BMisc::get_list_element(inner_extra_gt_returns, "bread")
    Xe_gt <- BMisc::get_list_element(inner_extra_gt_returns, "Xe")

    acrt_gt_inffunc_mat <- gt_results$inffunc
    biters <- ptep$biters
    bstrap <- ptep$bstrap
    alp <- ptep$alp

    # overall att results
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

    # overall acrt results
    overall_acrt <- weighted.mean(acrt.overall_gt, w = o_weights$overall_weight)
    overall_acrt_inffunc <- as.matrix(rowSums(sweep(acrt_gt_inffunc_mat, 2, o_weights$overall_weight, "*")) / sum(o_weights$overall_weight))
    overall_acrt_se <- getSE(overall_acrt_inffunc, biters = biters, alp = alp)

    # point estimates of ATT(d) and ACRT(d)
    att.d <- BMisc::weighted_combine_list(att.d_gt, o_weights$overall_weight)
    acrt.d <- BMisc::weighted_combine_list(acrt.d_gt, o_weights$overall_weight)

    # values of the dose
    dvals <- ptep$dvals
    degree <- ptep$degree
    knots <- ptep$knots
    bs_grid <- splines2::bSpline(dvals, degree = degree, knots = knots)
    bs_grid <- cbind(1, bs_grid) # add intercept
    bs_deriv <- splines2::dbs(dvals, degree = degree, knots = knots)
    bs_deriv <- cbind(0, bs_deriv) # intercept doesn't matter here, just placeholder to get dimensions right

    # since we are picking dvals over a grid, the only randomness comes from
    # estimating the \beta's
    n1_vec <- sapply(Xe_gt, nrow)
    n <- nrow(acrt_gt_inffunc_mat)
    # these are treated observations
    keep_mat <- acrt_gt_inffunc_mat != 0
    # these are both treated and control observations
    keep_mat2 <- overall_att_res$att_gt$inffunc != 0
    # these keep track of untreated comparison units
    keep_mat2 <- keep_mat2 & !keep_mat
    #
    comparison_inffunc <- overall_att_res$att_gt$inffunc
    comparison_inffunc[!keep_mat2] <- 0
    n0_vec <- colSums(keep_mat2)
    if (!all(colSums(keep_mat) == n1_vec)) {
        stop("something off with overall influence function")
    }

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
    biters <- ptep$biters
    alp <- ptep$alp
    cband <- ptep$cband
    boot_res <- mboot2(att.d_inffunc, biters = biters, alp = alp)
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
    acrt_boot_res <- mboot2(acrt.d_inffunc, biters = biters, alp = alp)
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
