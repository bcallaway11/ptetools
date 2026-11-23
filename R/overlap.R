# =============================================================================
# Title: Overlap diagnostics for group-time cells
# Description: check_overlap() reports, for each (g,t) cell, whether treated
#   units have comparison units with similar covariates: the share of
#   treated units outside the comparison group's covariate range and, where
#   it can be estimated, the share with a propensity score near 1 (#28).
#   Built on the same cells and covariates as pte_attgt().
# Author: Brant Callaway
# Last update: 2026-11-23
# Date created: 2026-11-20
# =============================================================================

#' @title Overlap Diagnostics for Group-Time Cells
#'
#' @description Covariate-adjusted estimates of \eqn{ATT(g,t)} need overlap
#'   in every group-time cell: each treated unit needs comparison units with
#'   similar covariates. \code{check_overlap()} reports, for each cell, how much
#'   of the treated group lacks that support.
#'
#'   The cells are exactly the ones \code{pte_default()} estimates with
#'   (same comparison group, base period and anticipation), and the
#'   covariates are built the same way as in \code{pte_attgt()}: the
#'   variables in \code{xformula} at the base period, plus changes in the
#'   variables in \code{d_covs_formula} and, with
#'   \code{lagged_outcome_cov = TRUE}, the base-period outcome.
#'
#'   Two measures are reported for each cell:
#'   \itemize{
#'     \item \strong{Range support}: the share of treated units with at least
#'       one covariate outside the comparison group's range (its minimum and
#'       maximum). For a binary covariate, this is a value no comparison unit
#'       has. Being inside the range is necessary, not sufficient, for
#'       overlap, but the check works in every cell.
#'     \item \strong{Propensity score}: the share of treated units whose
#'       estimated propensity score (a logit, as in \code{pte_attgt()}) is
#'       above each of \code{pscore_thresholds}. Reported only when the cell
#'       has more treated units than covariates (\code{pscore_ok}); with
#'       fewer, the logit separates whatever the overlap.
#'   }
#'
#'   With \code{weightsname}, every share is also reported weighted by the
#'   sampling weights (columns ending in \code{_w}). A few heavy treated
#'   units outside the comparison range can carry much of a weighted ATT.
#'
#'   This is a diagnostic only; it does not trim or change any estimate.
#'
#' @inheritParams pte_default
#' @param pscore_thresholds Propensity score thresholds. The default
#'   \code{c(0.9, 0.999)} covers the usual trimming rule of thumb (Crump et
#'   al., 2009) and the default of \code{did::trimmer()}.
#'
#' @return A data frame of class \code{pte_overlap}, one row per group-time
#'   cell, with columns
#'   \describe{
#'     \item{group, time.period}{the cell, in units of \code{tname}}
#'     \item{post}{whether the cell is post-treatment}
#'     \item{n_treated, n_comparison}{numbers of treated and comparison units}
#'     \item{n_covariates}{number of linearly independent covariates,
#'       including the intercept}
#'     \item{pscore_ok}{whether the propensity score can be estimated
#'       (more treated units than covariates)}
#'     \item{outside_range}{share of treated units outside the comparison
#'       range on at least one covariate}
#'     \item{worst_covariate}{the covariate with the most treated units
#'       outside the comparison range (\code{NA} if none)}
#'     \item{pscore_above_0.9, pscore_above_0.999}{share of treated units
#'       with a propensity score above each threshold, one column per
#'       element of \code{pscore_thresholds} (these names are for the
#'       default); \code{NA} if \code{pscore_ok} is \code{FALSE}}
#'   }
#'   plus weighted versions of the shares (ending in \code{_w}) when
#'   \code{weightsname} is given. With \code{xformula = ~1} and no other
#'   covariates there is nothing to check, and the shares are \code{NA}.
#'
#' @references Crump, R. K., Hotz, V. J., Imbens, G. W. and Mitnik, O. A.
#'   (2009). Dealing with limited overlap in estimation of average treatment
#'   effects. \emph{Biometrika}, 96(1), 187-199.
#'
#' @examples
#' ov <- check_overlap(
#'   yname = "lemp", gname = "first.treat", tname = "year",
#'   idname = "countyreal", data = did::mpdta, xformula = ~lpop
#' )
#' ov
#'
#' @export
check_overlap <- function(yname,
                          gname,
                          tname,
                          idname,
                          data,
                          xformula = ~1,
                          d_covs_formula = ~ -1,
                          lagged_outcome_cov = FALSE,
                          weightsname = NULL,
                          control_group = "notyettreated",
                          base_period = "varying",
                          anticipation = 0,
                          panel = TRUE,
                          pscore_thresholds = c(0.9, 0.999)) {
  if (!isTRUE(panel)) {
    stop("check_overlap() supports panel data only so far.", call. = FALSE)
  }
  if (!is.numeric(pscore_thresholds) || any(pscore_thresholds <= 0) ||
        any(pscore_thresholds >= 1)) {
    stop("pscore_thresholds must be numbers strictly between 0 and 1.",
         call. = FALSE)
  }

  # the same setup and cells as pte_default(); no estimation or bootstrap
  ptep <- setup_pte(
    yname = yname, gname = gname, tname = tname, idname = idname,
    data = data, panel = panel, anticipation = anticipation,
    base_period = base_period, control_group = control_group,
    weightsname = weightsname, biters = 0
  )
  res <- compute.pte(
    ptep = ptep,
    subset_fun = two_by_two_subset,
    attgt_fun = overlap_attgt,
    xformula = xformula,
    d_covs_formula = d_covs_formula,
    lagged_outcome_cov = lagged_outcome_cov,
    pscore_thresholds = pscore_thresholds,
    control_group = control_group,
    base_period = base_period,
    anticipation = anticipation
  )

  # one row per cell; reference cells of a universal base period (no
  # diagnostics) are left out
  rows <- lapply(res$extra_gt_returns, function(egr) {
    if (is.null(egr$extra_gt_returns)) return(NULL)
    cbind(
      data.frame(group = pte_calendar_time(ptep$data, egr$group),
                 time.period = pte_calendar_time(ptep$data, egr$time.period)),
      egr$extra_gt_returns
    )
  })
  out <- do.call(rbind, rows)
  out$post <- out$time.period >= out$group
  out <- out[, c("group", "time.period", "post",
                 setdiff(names(out), c("group", "time.period", "post")))]
  if (is.null(weightsname)) out <- out[, !grepl("_w$", names(out))]
  rownames(out) <- NULL
  attr(out, "weighted") <- !is.null(weightsname)
  class(out) <- c("pte_overlap", "data.frame")
  out
}

# --- Diagnostics for one cell ------------------------------------------------
#
# Internal attgt_fun for check_overlap(): returns no estimate, only the cell's
# overlap diagnostics in extra_gt_returns (a one-row data frame). Covariates
# are built exactly as in pte_attgt(), and collinear columns are dropped the
# same way.
#
# Arguments:
#   gt_data            - gt_data_frame for this cell
#   xformula, d_covs_formula, lagged_outcome_cov - as in pte_attgt()
#   pscore_thresholds  - propensity score thresholds
#   ...                - not used
#
# Returns: attgt_noif object
overlap_attgt <- function(gt_data, xformula, d_covs_formula = ~ -1,
                          lagged_outcome_cov = FALSE,
                          pscore_thresholds = c(0.9, 0.999), ...) {
  pp <- gt_pre_post(gt_data)
  covmat <- gt_covariates(pp, xformula, d_covs_formula, lagged_outcome_cov)
  D <- pp$pre$D # nolint: object_name_linter (treatment indicator)
  w <- pp$pre$.w
  if (is.null(w)) w <- rep(1, length(D))
  treated <- D == 1
  n1 <- sum(treated)
  n0 <- sum(!treated)

  # Range support uses every covariate, before collinear columns are
  # dropped: a column that is constant among comparison units (e.g. the
  # dummy for a region with no comparison units) is exactly a support
  # failure, and pte_attgt() drops it (it cannot be estimated).
  x <- covmat[, colnames(covmat) != "(Intercept)", drop = FALSE]

  # drop collinear columns among comparison units, as pte_attgt() does
  covmat0 <- covmat[!treated, , drop = FALSE]
  qr0 <- qr(crossprod(covmat0) / max(n0, 1))
  covmat <- covmat[, qr0$pivot[seq_len(qr0$rank)], drop = FALSE]
  p <- ncol(covmat)

  # shares of treated units, unweighted and weighted
  share <- function(flag) mean(flag)
  share_w <- function(flag) sum(w[treated] * flag) / sum(w[treated])

  # --- range support ---------------------------------------------------------
  if (ncol(x) == 0 || n0 == 0) {
    outside <- NA
    worst <- NA_character_
  } else {
    lo <- apply(x[!treated, , drop = FALSE], 2, min)
    hi <- apply(x[!treated, , drop = FALSE], 2, max)
    x1 <- x[treated, , drop = FALSE]
    out_by_cov <- sweep(x1, 2, lo, "<") | sweep(x1, 2, hi, ">")
    outside <- rowSums(out_by_cov) > 0
    counts <- colSums(w[treated] * out_by_cov)
    worst <- if (any(counts > 0)) colnames(x)[which.max(counts)] else
      NA_character_
  }

  # --- propensity score ------------------------------------------------------
  # Only informative with more treated units than covariates; with fewer
  # the logit separates whatever the overlap. Same model as pte_attgt()
  # (quasibinomial: same fit, no warning for non-integer weights).
  pscore_ok <- n1 > p && n0 > 0
  pscore <- NULL
  if (pscore_ok && p > 1) {
    fit <- suppressWarnings(stats::glm(
      D ~ covmat - 1, family = stats::quasibinomial(link = "logit"),
      weights = w / mean(w)
    ))
    pscore <- stats::fitted(fit)[treated]
  }

  diag <- data.frame(n_treated = n1, n_comparison = n0, n_covariates = p,
                     pscore_ok = pscore_ok,
                     outside_range = if (anyNA(outside)) NA_real_ else
                       share(outside),
                     outside_range_w = if (anyNA(outside)) NA_real_ else
                       share_w(outside),
                     worst_covariate = worst)
  for (thr in pscore_thresholds) {
    nm <- paste0("pscore_above_", thr)
    diag[[nm]] <- if (is.null(pscore)) NA_real_ else share(pscore > thr)
    diag[[paste0(nm, "_w")]] <- if (is.null(pscore)) NA_real_ else
      share_w(pscore > thr)
  }
  attgt_noif(attgt = NA_real_, extra_gt_returns = diag)
}

# --- Print method ------------------------------------------------------------

#' @title Print Overlap Diagnostics
#'
#' @description Prints a short summary of \code{check_overlap()} results: how
#'   many post-treatment cells have treated units without comparison
#'   support, and the worst of them. The full table is the object itself
#'   (a data frame).
#'
#' @param x a \code{pte_overlap} object
#' @param n number of cells to show
#' @param ... not used
#'
#' @return \code{x}, invisibly
#'
#' @keywords internal
#' @export
print.pte_overlap <- function(x, n = 5, ...) {
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  weighted <- isTRUE(attr(x, "weighted"))
  col <- if (weighted) "outside_range_w" else "outside_range"
  post <- df[df$post, , drop = FALSE]
  cat(sprintf(
    "Overlap diagnostics for %d group-time cells (%d post-treatment)\n",
    nrow(df), nrow(post)
  ))
  if (all(is.na(df$outside_range))) {
    cat("No covariates to check (xformula = ~1).\n")
    return(invisible(x))
  }
  bad <- post[!is.na(post[[col]]) & post[[col]] > 0, , drop = FALSE]
  cat("Post-treatment cells with treated units outside the comparison",
      "range:", nrow(bad), "\n")
  no_ps <- sum(!post$pscore_ok)
  if (no_ps > 0) {
    cat("Post-treatment cells with too few treated units for a propensity",
        "score:", no_ps, "\n")
  }
  if (nrow(bad) > 0) {
    bad <- bad[order(-bad[[col]]), , drop = FALSE]
    show <- c("group", "time.period", "n_treated", "n_comparison",
              "outside_range", if (weighted) "outside_range_w",
              "worst_covariate",
              grep("^pscore_above_", names(df), value = TRUE))
    if (!weighted) show <- show[!grepl("_w$", show)]
    heading <- if (weighted) "Worst cells (by weighted share):" else
      "Worst cells:"
    cat("\n", heading, "\n", sep = "")
    print(utils::head(bad[, show], n), row.names = FALSE, digits = 3)
  }
  invisible(x)
}
