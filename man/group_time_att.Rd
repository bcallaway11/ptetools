% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{group_time_att}
\alias{group_time_att}
\title{Class for Estimates across Groups and Time}
\usage{
group_time_att(
  group,
  time.period,
  att,
  V_analytical,
  se,
  crit_val,
  inf_func,
  n,
  W,
  Wpval,
  cband,
  alp,
  ptep,
  extra_gt_returns
)
}
\arguments{
\item{group}{numeric vector of groups for ATT(g,t)}

\item{time.period}{numeric vector of time periods for ATT(g,t)}

\item{att}{numeric vector containing the value of ATT(g,t) for
corresponding group and time period}

\item{V_analytical}{analytical asymptotic variance matrix for ATT(g,t)'s}

\item{se}{numeric vector of standard errors}

\item{crit_val}{critical value (usually a critical value for conducting
uniform inference)}

\item{inf_func}{matrix of influence function}

\item{n}{number of unique individuals}

\item{W}{Wald statistic for ATT(g,t) version of pre-test of parallel trends
assumption}

\item{Wpval}{p-value for Wald pre-test of ATT(g,t) version of parallel
trends assumption}

\item{cband}{logical indicating whether or not to report a confidence band}

\item{alp}{significance level}

\item{ptep}{\code{pte_params} object}

\item{extra_gt_returns}{list containing extra returns at the group-time level}
}
\value{
object of class \code{group_time_att}
}
\description{
Class that holds causal effect parameter estimates across
timing groups and time periods
}
