process_dose_gt <- function(gt_results, ptep, ...) {
    # make the call to att, to get same format of results
    att_gt <- process_att_gt(gt_results, ptep)
    o_weights <- overall_weights(att_gt, ...)

    # main dose-specific results are in extra_gt_returns
    all_extra_gt_returns <- att_gt$extra_gt_returns
    groups <- unlist(BMisc::getListElement(all_extra_gt_returns, "group"))
    time.periods <- unlist(BMisc::getListElement(all_extra_gt_returns, "time.period"))

    # check that order of groups and time periods matches
    if (!all(cbind(groups, time.periods) == o_weights[, c("group", "time.period")])) {
        stop("in processing dose results, mismatch between order of groups and time periods")
    }

    inner_extra_gt_returns <- BMisc::getListElement(all_extra_gt_returns, "extra_gt_returns")
    att.d_gt <- BMisc::getListElement(inner_extra_gt_returns, "att.d")
    acrt.d_gt <- BMisc::getListElement(inner_extra_gt_returns, "acrt.d")
    att.overall_gt <- unlist(BMisc::getListElement(inner_extra_gt_returns, "att.overall"))
    acrt.overall_gt <- unlist(BMisc::getListElement(inner_extra_gt_returns, "acrt.overall"))
    bet_gt <- BMisc::getListElement(inner_extra_gt_returns, "bet")
    bread_gt <- BMisc::getListElement(inner_extra_gt_returns, "bread")
    Xe_gt <- BMisc::getListElement(inner_extra_gt_returns, "Xe")

    # point estimates of ATT(d) and ACRT(d)
    att.d <- weighted_combine_list(att.d_gt, o_weights$overall_weight)
    acrt.d <- weighted_combine_list(acrt.d_gt, o_weights$overall_weight)

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
    acrt_gt_inffunc_mat <- gt_results$inffunc
    n <- nrow(acrt_gt_inffunc_mat)
    keep_mat <- acrt_gt_inffunc_mat != 0
    if (!all(colSums(keep_mat) == n1_vec)) {
        stop("something off with overall influence function")
    }

    att.d_gt_inffunc <- lapply(
        1:length(Xe_gt),
        function(i) {
            out_inffunc <- matrix(data = 0, nrow = n, ncol = length(dvals))
            this_inffunc <- (Xe_gt[[i]] %*% bread_gt[[i]] %*% t(bs_grid))
            out_inffunc[keep_mat[, i], ] <- (n / n1_vec[i]) * this_inffunc
            out_inffunc
        }
    )

    att.d_inffunc <- weighted_combine_list(att.d_gt_inffunc, o_weights$overall_weight)
    biters <- ptep$biters
    alp <- ptep$alp
    cband <- ptep$cband
    boot_res <- mboot2(att.d_inffunc, biters = biters, alp = alp)
    att.d_se <- boot_res$boot_se
    if (cband) {
        att.d_crit.val <- boot_res$crit_val
        att.d_crit.val <- crit_val_checks(att.d_crit.val, alp)
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
    acrt.d_inffunc <- weighted_combine_list(acrt.d_gt_inffunc, o_weights$overall_weight)
    acrt_boot_res <- mboot2(acrt.d_inffunc, biters = biters, alp = alp)
    acrt.d_se <- acrt_boot_res$boot_se
    if (cband) {
        acrt.d_crit.val <- acrt_boot_res$crit_val
        acrt.d_crit.val <- crit_val_checks(acrt.d_crit.val, alp)
    } else {
        acrt.d_crit.val <- qnorm(1 - alp / 2)
    }

    # placeholder for tracking `call`
    call <- NULL

    dose_obj(
        dose = dvals,
        att.d = att.d,
        att.d_se = att.d_se,
        att.d_crit.val = att.d_crit.val,
        att.d_inffunc = att.d_inffunc,
        acrt.d = acrt.d,
        acrt.d_se = acrt.d_se,
        acrt.d_crit.val = acrt.d_crit.val,
        acrt.d_inffunc = acrt.d_inffunc,
        pte_params = ptep,
        call = call
    )
}

#' @title dose_obj
#'
#' @description Holds results from computing dose-specific treatment effects
#'  with a continuous treatment
#'
#' @param dose vector containing the values of the dose used in estimation
#' @param att.d estimates of ATT(d) for each value of `dose`
#' @param att.d_se standard error of ATT(d) for each value of `dose`
#' @param att.d_crt.val critical value to produce pointwise or uniform confidence
#'  interval for ATT(d)
#' @param att.d_inffunc matrix containing the influence function from estimating
#'  ATT(d)
#' @param acrt.d estimates of ACRT(d) for each value of `dose`
#' @param acrt.d_se standard error of ACRT(d) for each value of `dose`
#' @param acrt.d_crt.val critical value to produce pointwise or uniform confidence
#'  interval for ACRT(d)
#' @param acrt.d_inffunc matrix containing the influence function from estimating
#'  ACRT(d)
#' @param pte_params a pte_params object containing other parameters passed to the function
#' @param call the original call to the function for computing causal effect parameters
#'  with a continuous treatment
#'
#' @return dose_obj
#'
#' @export
dose_obj <- function(
    dose,
    att.d = NULL,
    att.d_se = NULL,
    att.d_crit.val = NULL,
    att.d_inffunc = NULL,
    acrt.d = NULL,
    acrt.d_se = NULL,
    acrt.d_crit.val = NULL,
    acrt.d_inffunc = NULL,
    pte_params = NULL,
    call = NULL) {
    out <- list(
        dose = dose,
        att.d = att.d,
        att.d_se = att.d_se,
        att.d_crit.val = att.d_crit.val,
        att.d_inffunc = att.d_inffunc,
        acrt.d = acrt.d,
        acrt.d_se = acrt.d_se,
        acrt.d_crit.val = acrt.d_crit.val,
        acrt.d_inffunc = acrt.d_inffunc,
        pte_params = pte_params,
        call = call
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
#' @export
summary.dose_obj <- function(object, ...) {
    dose_obj <- object
    out <- list(
        overall_att = dose_obj$overall_att,
        overall_att_se = dose_obj$overall_att_se,
        overall_acrt = dose_obj$overall_acrt,
        overall_acrt_se = dose_obj$overall_acrt_se,
        dose = dose_obj$dose,
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
#' @export
print.summary.dose_obj <- function(x, ...) {
    # browser()
    alp <- x$alp
    cband <- x$cband
    bstrap <- TRUE # TODO: hardcoded because this only option

    # ATT(d)
    cat("ATT(d):\n")

    cband_text1a <- paste0(100 * (1 - alp), "% ")
    cband_text1b <- ifelse(bstrap,
        ifelse(cband, "Simult. ", "Pointwise "),
        "Pointwise "
    )
    cband_text1 <- paste0("[", cband_text1a, cband_text1b)

    cband_lower <- x$att.d - x$att.d_crit.val * x$att.d_se
    cband_upper <- x$att.d + x$att.d_crit.val * x$att.d_se

    sig <- (cband_upper < 0) | (cband_lower > 0)
    sig[is.na(sig)] <- FALSE
    sig_text <- ifelse(sig, "*", "")

    out <- cbind.data.frame(x$dose, x$att.d, x$att.d_se, cband_lower, cband_upper)
    out <- round(out, 4)
    out <- cbind.data.frame(out, sig_text)


    colnames(out) <- c("dose", "ATT(d)", "Std. Error", cband_text1, "Conf. Band]", "")
    print(out, row.names = FALSE, justify = "centre")
    cat("\n\n")

    # ATT(d)
    cat("ACRT(d):\n")

    cband_text1a <- paste0(100 * (1 - alp), "% ")
    cband_text1b <- ifelse(bstrap,
        ifelse(cband, "Simult. ", "Pointwise "),
        "Pointwise "
    )
    cband_text1 <- paste0("[", cband_text1a, cband_text1b)

    cband_lower <- x$acrt.d - x$acrt.d_crit.val * x$acrt.d_se
    cband_upper <- x$acrt.d + x$acrt.d_crit.val * x$acrt.d_se

    sig <- (cband_upper < 0) | (cband_lower > 0)
    sig[is.na(sig)] <- FALSE
    sig_text <- ifelse(sig, "*", "")

    out <- cbind.data.frame(x$dose, x$acrt.d, x$acrt.d_se, cband_lower, cband_upper)
    out <- round(out, 4)
    out <- cbind.data.frame(out, sig_text)


    colnames(out) <- c("dose", "ACRT(d)", "Std. Error", cband_text1, "Conf. Band]", "")
    print(out, row.names = FALSE, justify = "centre")
    cat("---\n")
    cat("Signif. codes: `*' confidence band does not cover 0")
    cat("\n\n")
}
