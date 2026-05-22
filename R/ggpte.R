# =============================================================================
# Title: Plotting methods for ptetools result classes
# Description: autoplot and plot S3 methods for pte_qtt and pte_results.
#   autoplot returns a ggplot object the caller can modify with +.
#   plot is a convenience wrapper that prints the autoplot result.
# Author: Brant Callaway
# Last update: 2026-05-22
# Date created: 2026-05-22
# =============================================================================

# --- pte_qtt -----------------------------------------------------------------

#' @title autoplot.pte_qtt
#'
#' @description Plot a \code{pte_qtt} object. The overall aggregation shows
#'   the QTT curve across quantiles with a confidence ribbon. The ribbon is
#'   omitted when standard errors are \code{NA} (e.g., when \code{biters = 0}).
#'
#' @param object a \code{pte_qtt} object
#' @param type which aggregation to plot. Currently only \code{"overall"} is
#'   implemented; \code{"dynamic"} and \code{"group"} are stubs.
#' @param cband logical; if \code{TRUE} (default), show the uniform confidence
#'   band. If \code{FALSE}, show pointwise confidence intervals instead.
#' @param ... unused
#'
#' @return a \code{ggplot} object
#' @export
autoplot.pte_qtt <- function(object, type = "overall", cband = TRUE, ...) {
  if (type == "overall") {
    df <- object$overall
    lower_col <- if (cband) "lower_ub" else "lower_pw"
    upper_col <- if (cband) "upper_ub" else "upper_pw"
    has_ci <- !anyNA(df[[lower_col]]) && !anyNA(df[[upper_col]])

    p <- ggplot(df, aes(x = probs, y = qtt)) +
      geom_hline(yintercept = 0, color = "gray50") +
      geom_line() +
      geom_point() +
      scale_x_continuous("Quantile", limits = c(0, 1)) +
      scale_y_continuous("QTT") +
      theme_bw()

    if (has_ci) {
      p <- p +
        geom_line(aes(y = .data[[lower_col]]), linetype = "dashed") +
        geom_line(aes(y = .data[[upper_col]]), linetype = "dashed")
    }

    return(p)
  }

  # stubs for other aggregations
  if (type == "dynamic") stop("autoplot for type='dynamic' not yet implemented")
  if (type == "group")   stop("autoplot for type='group' not yet implemented")
  stop("unknown type: ", type)
}


#' @title plot.pte_qtt
#'
#' @description Convenience wrapper around \code{\link{autoplot.pte_qtt}}.
#'
#' @param x a \code{pte_qtt} object
#' @param type which aggregation to plot. See \code{\link{autoplot.pte_qtt}}.
#' @param ... passed to \code{autoplot.pte_qtt}
#'
#' @return invisibly returns the \code{ggplot} object
#' @export
plot.pte_qtt <- function(x, type = "overall", ...) {
  p <- autoplot(x, type = type, ...)
  print(p)
  invisible(p)
}


# --- pte_results -------------------------------------------------------------

#' @title ptetools Generic Plotting Function
#'
#' @description The main plotting function in the `ptetools` package.  It plots
#'  event studies.  This
#'  function is generic enough that most packages that otherwise use
#'  the `ptetools` package can call it directly to plot an event study.
#'
#' @param pte_results A \code{pte_results} object
#'
#' @return A ggplot object
#' @export
ggpte <- function(pte_results) {
  plot_df <- summary(pte_results)$event_study
  colnames(plot_df) <- c("e", "att", "se", "cil", "ciu")
  plot_df$post <- as.factor(1 * (plot_df$e >= 0))
  ggplot(plot_df, aes(x = e, y = att)) +
    geom_line(aes(color = post)) +
    geom_point(aes(color = post)) +
    geom_line(aes(y = ciu), linetype = "dashed", alpha = 0.5) +
    geom_line(aes(y = cil), linetype = "dashed", alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "bottom")
}


#' @title Generic Plots with a Continuous Treatment
#'
#' @description Plots dose-specific results in applications with a continuous treatment
#'
#' @param dose_obj a `dose_obj` that holds results with a continuous treatment
#' @param type whether to plot ATT(d) or ACRT(d), defaults to `att` for
#'  plotting ATT(d).  For ACRT(d), use "acrt"
#'
#' @return A ggplot object
#' @export
ggpte_cont <- function(dose_obj, type = "att") {
  dose <- dose_obj$dose
  if (type == "acrt") {
    acrt.d <- dose_obj$acrt.d
    acrt.d_se <- dose_obj$acrt.d_se
    acrt.d_crit.val <- dose_obj$acrt.d_crit.val
    plot_df <- cbind.data.frame(dose, acrt.d, acrt.d_se, acrt.d_crit.val)
    ggplot(plot_df, aes(x = dose, y = acrt.d)) +
      geom_line(size = 2) +
      geom_ribbon(
        aes(
          ymin = acrt.d - acrt.d_crit.val * acrt.d_se,
          ymax = acrt.d + acrt.d_crit.val * acrt.d_se
        ),
        fill = "lightgray", alpha = 0.5
      ) +
      theme_bw()
  } else { # att(d) plot
    att.d <- dose_obj$att.d
    att.d_se <- dose_obj$att.d_se
    att.d_crit.val <- dose_obj$att.d_crit.val
    plot_df <- cbind.data.frame(dose, att.d, att.d_se, att.d_crit.val)
    ggplot(plot_df, aes(x = dose, y = att.d)) +
      geom_line(size = 2) +
      geom_ribbon(
        aes(
          ymin = att.d - att.d_crit.val * att.d_se,
          ymax = att.d + att.d_crit.val * att.d_se
        ),
        fill = "lightgray", alpha = 0.5
      ) +
      theme_bw()
  }
}
