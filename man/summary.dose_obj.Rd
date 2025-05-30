% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_dose_gt.R
\name{summary.dose_obj}
\alias{summary.dose_obj}
\title{summary.dose_obj}
\usage{
\method{summary}{dose_obj}(object, ...)
}
\arguments{
\item{object}{an \code{dose_obj} object}

\item{...}{extra arguments}
}
\value{
a list containing the summary of a \code{dose_obj} object:
\itemize{
\item \code{dose}: vector of dose values
\item \code{overall_att}: overall ATT estimate
\item \code{overall_att_se}: standard error of overall ATT estimate
\item \code{overall_acrt}: overall ACRT estimate
\item \code{overall_acrt_se}: standard error of overall ACRT estimate
\item \code{att.d}: vector of ATT(d) estimates
\item \code{att.d_se}: vector of standard errors for ATT(d) estimates
\item \code{att.d_crit.val}: critical value for pointwise or uniform confidence interval for ATT(d)
\item \code{acrt.d}: vector of ACRT(d) estimates
\item \code{acrt.d_se}: vector of standard errors for ACRT(d) estimates
\item \code{acrt.d_crit.val}: critical value for pointwise or uniform confidence interval for ACRT(d)
\item \code{alp}: significance level
\item \code{cband}: logical indicating whether to use simultaneous or pointwise confidence intervals
\item \code{bstrap}: logical indicating whether to use bootstrap for critical value
}
}
\description{
summarizes a \code{dose_obj} object
}
\keyword{internal}
