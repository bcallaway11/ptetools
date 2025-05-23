% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{attgt_if}
\alias{attgt_if}
\title{Class for (g,t)-Specific Results with Influence Function}
\usage{
attgt_if(attgt, inf_func, extra_gt_returns = NULL)
}
\arguments{
\item{attgt}{group-time average treatment effect}

\item{inf_func}{influence function}

\item{extra_gt_returns}{A place to return anything extra from particular
group-time average treatment effect calculations.  For DID, this might
be something like propensity score estimates, regressions of untreated
potential outcomes on covariates.  For ife, this could be something
like the first step regression 2sls estimates.  This argument is also
potentially useful for debugging.}
}
\value{
\code{attgt_if} object
}
\description{
Class for holding group-time average treatment effects
along with their influence function
}
