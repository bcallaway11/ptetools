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
