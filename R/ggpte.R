# =============================================================================
# Title: Plotting methods for ptetools result classes
# Description: autoplot and plot S3 methods for pte_qtt and pte_results.
#   autoplot returns a ggplot object the caller can modify with +.
#   plot is a convenience wrapper that prints the autoplot result.
# Author: Brant Callaway
# Last update: 2026-07-25
# Date created: 2026-05-22
# =============================================================================

# --- pte_qtt -----------------------------------------------------------------

#' @title autoplot.pte_qtt
#'
#' @description Plot a \code{pte_qtt} object.
#'
#' For \code{type = "overall"}: QTT curve with quantile on the x-axis.
#'
#' For \code{type = "dynamic"}: event-study plot with event time on the x-axis.
#'   Each selected quantile is a separate colored line. CIs are shown by default
#'   when a single quantile is plotted, and suppressed by default when multiple
#'   quantiles are plotted.
#'
#' @param object a \code{pte_qtt} object
#' @param type which aggregation to plot: \code{"overall"} (default) or
#'   \code{"dynamic"}. \code{"group"} is a stub.
#' @param cband logical; if \code{TRUE} (default), show uniform confidence band;
#'   if \code{FALSE}, show pointwise intervals. Applies when CIs are displayed.
#' @param plot_probs numeric vector of quantile levels to show in the dynamic
#'   plot. Defaults to \code{0.5} (median). All values must be present in
#'   \code{object$dynamic$probs}.
#' @param plot_ci logical or \code{NULL}. If \code{NULL} (default), CIs are
#'   shown when \code{length(plot_probs) == 1} and suppressed otherwise. Set
#'   \code{TRUE} to always show CIs, \code{FALSE} to never show them.
#' @param ... unused
#'
#' @return a \code{ggplot} object
#' @export
autoplot.pte_qtt <- function(object, type = "overall", cband = TRUE,
                             plot_probs = 0.5, plot_ci = NULL, ...) {
  lower_col <- if (cband) "lower_ub" else "lower_pw"
  upper_col <- if (cband) "upper_ub" else "upper_pw"

  if (type == "overall") {
    df <- object$overall
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

  if (type == "dynamic") {
    available <- unique(object$dynamic$probs)
    missing_p <- setdiff(plot_probs, available)
    if (length(missing_p) > 0) {
      stop("plot_probs value(s) not found in object$dynamic$probs: ",
           paste(missing_p, collapse = ", "))
    }

    if (is.null(plot_ci)) plot_ci <- length(plot_probs) == 1

    df <- object$dynamic[object$dynamic$probs %in% plot_probs, ]
    has_ci <- plot_ci && !anyNA(df[[lower_col]]) && !anyNA(df[[upper_col]])

    if (length(plot_probs) == 1) {
      df$post <- factor(df$e >= 0, levels = c(FALSE, TRUE),
                        labels = c("Pre", "Post"))
      p <- ggplot(df, aes(x = e, y = qtt, color = post)) +
        geom_hline(yintercept = 0, color = "gray50") +
        geom_vline(xintercept = -0.5, color = "gray50", linetype = "dashed") +
        geom_point() +
        scale_x_continuous("Event Time") +
        scale_y_continuous("QTT") +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())

      if (has_ci) {
        p <- p +
          geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
                        width = 0.2)
      }
    } else {
      df$probs <- as.factor(df$probs)
      p <- ggplot(df, aes(x = e, y = qtt, color = probs, group = probs)) +
        geom_hline(yintercept = 0, color = "gray50") +
        geom_vline(xintercept = -0.5, color = "gray50", linetype = "dashed") +
        geom_line() +
        geom_point() +
        scale_x_continuous("Event Time") +
        scale_y_continuous("QTT") +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())

      if (has_ci) {
        p <- p +
          geom_line(mapping = aes(x = e, group = probs, y = .data[[lower_col]]),
                    linetype = "dashed", color = "gray40", inherit.aes = FALSE) +
          geom_line(mapping = aes(x = e, group = probs, y = .data[[upper_col]]),
                    linetype = "dashed", color = "gray40", inherit.aes = FALSE)
      }
    }

    return(p)
  }

  if (type == "group") stop("autoplot for type='group' not yet implemented")
  stop("unknown type: ", type)
}


#' @title plot.pte_qtt
#'
#' @description Convenience wrapper around \code{\link{autoplot.pte_qtt}}.
#'
#' @param x a \code{pte_qtt} object
#' @param type which aggregation to plot. See \code{\link{autoplot.pte_qtt}}.
#' @param cband logical; if \code{TRUE} (default), show uniform confidence band.
#' @param plot_probs numeric vector of quantile levels to show. See \code{\link{autoplot.pte_qtt}}.
#' @param plot_ci logical or \code{NULL}. See \code{\link{autoplot.pte_qtt}}.
#' @param ... passed to \code{autoplot.pte_qtt}
#'
#' @return invisibly returns the \code{ggplot} object
#' @export
plot.pte_qtt <- function(x, type = "overall", cband = TRUE,
                         plot_probs = 0.5, plot_ci = NULL, ...) {
  p <- autoplot(x, type = type, cband = cband,
                plot_probs = plot_probs, plot_ci = plot_ci, ...)
  print(p)
  invisible(p)
}


# --- pte_results -------------------------------------------------------------

#' @title autoplot.pte_results
#'
#' @description Event-study plot for a \code{pte_results} object. Pre- and
#'   post-treatment periods are distinguished by color.
#'
#' @param object a \code{pte_results} object
#' @param ... unused
#'
#' @return a \code{ggplot} object
#' @export
autoplot.pte_results <- function(object, ...) {
  plot_df <- summary(object)$event_study
  colnames(plot_df) <- c("e", "att", "se", "cil", "ciu")
  plot_df$post <- factor(plot_df$e >= 0, levels = c(FALSE, TRUE),
                         labels = c("Pre", "Post"))
  ggplot(plot_df, aes(x = e, y = att)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_line(aes(color = post)) +
    geom_point(aes(color = post)) +
    geom_line(aes(y = ciu), linetype = "dashed") +
    geom_line(aes(y = cil), linetype = "dashed") +
    scale_x_continuous("Event Time") +
    scale_y_continuous("ATT") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())
}


#' @title autoplot.pte_emp_boot
#'
#' @description Event-study plot for a \code{pte_emp_boot} object returned by
#'   empirical-bootstrap estimators (e.g., \code{cic()}, \code{qdid()},
#'   \code{mdid()}). Pre- and post-treatment periods are distinguished by color.
#'
#' @param object a \code{pte_emp_boot} object
#' @param ... unused
#'
#' @return a \code{ggplot} object
#' @export
autoplot.pte_emp_boot <- function(object, ...) {
  autoplot.pte_results(object, ...)
}


#' @title plot.pte_results
#'
#' @description Convenience wrapper around \code{\link{autoplot.pte_results}}.
#'
#' @param x a \code{pte_results} object
#' @param ... passed to \code{autoplot.pte_results}
#'
#' @return invisibly returns the \code{ggplot} object
#' @export
plot.pte_results <- function(x, ...) {
  p <- autoplot(x, ...)
  print(p)
  invisible(p)
}


#' @title plot.pte_emp_boot
#'
#' @description Convenience wrapper around \code{\link{autoplot.pte_emp_boot}}.
#'
#' @param x a \code{pte_emp_boot} object
#' @param ... passed to \code{autoplot.pte_emp_boot}
#'
#' @return invisibly returns the \code{ggplot} object
#' @export
plot.pte_emp_boot <- function(x, ...) {
  p <- autoplot(x, ...)
  print(p)
  invisible(p)
}


#' @title ggpte
#'
#' @description Deprecated. Use \code{autoplot()} on the \code{pte_results}
#'   object instead.
#'
#' @param pte_results a \code{pte_results} object
#'
#' @return a \code{ggplot} object
#' @export
ggpte <- function(pte_results) {
  .Deprecated("autoplot")
  autoplot(pte_results)
}


# --- dose_obj ----------------------------------------------------------------

#' @title autoplot.dose_obj
#'
#' @description Plot dose-specific results for a continuous treatment.
#'
#' @param object a \code{dose_obj} object
#' @param type whether to plot \code{"att"} (default) or \code{"acrt"}
#' @param ... unused
#'
#' @return a \code{ggplot} object
#' @export
autoplot.dose_obj <- function(object, type = "att", ...) {
  dose <- object$dose
  if (type == "acrt") {
    # as.numeric() strips any stray attributes (e.g. the "cband" attribute
    # left by crit_val_checks(), or quantile()'s "95%"-style names) -- a
    # length-1 value carrying extra attributes does not recycle correctly
    # in data.frame()/cbind.data.frame() alongside the length(dose) columns
    plot_df <- cbind.data.frame(
      dose,
      est     = object$acrt.d,
      se      = object$acrt.d_se,
      crit    = as.numeric(object$acrt.d_crit.val)
    )
    ggplot(plot_df, aes(x = dose, y = est)) +
      geom_ribbon(aes(ymin = est - crit * se, ymax = est + crit * se),
                  fill = "lightgray", alpha = 0.5) +
      geom_line(linewidth = 1) +
      scale_x_continuous("Dose") +
      scale_y_continuous("ACRT(d)") +
      theme_bw()
  } else {
    plot_df <- cbind.data.frame(
      dose,
      est     = object$att.d,
      se      = object$att.d_se,
      crit    = as.numeric(object$att.d_crit.val)
    )
    ggplot(plot_df, aes(x = dose, y = est)) +
      geom_ribbon(aes(ymin = est - crit * se, ymax = est + crit * se),
                  fill = "lightgray", alpha = 0.5) +
      geom_line(linewidth = 1) +
      scale_x_continuous("Dose") +
      scale_y_continuous("ATT(d)") +
      theme_bw()
  }
}


#' @title plot.dose_obj
#'
#' @description Convenience wrapper around \code{\link{autoplot.dose_obj}}.
#'
#' @param x a \code{dose_obj} object
#' @param ... passed to \code{autoplot.dose_obj}
#'
#' @return invisibly returns the \code{ggplot} object
#' @export
plot.dose_obj <- function(x, ...) {
  p <- autoplot(x, ...)
  print(p)
  invisible(p)
}


#' @title ggpte_cont
#'
#' @description Deprecated. Use \code{autoplot()} on the \code{dose_obj}
#'   instead.
#'
#' @param dose_obj a \code{dose_obj} object
#' @param type whether to plot \code{"att"} (default) or \code{"acrt"}
#'
#' @return a \code{ggplot} object
#' @export
ggpte_cont <- function(dose_obj, type = "att") {
  .Deprecated("autoplot")
  autoplot(dose_obj, type = type)
}
