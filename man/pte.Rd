% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imports.R, R/pte.R
\docType{package}
\name{pte}
\alias{ptetools}
\alias{ptetools-package}
\alias{pte}
\title{Panel Treatment Effects}
\usage{
pte(
  yname,
  gname,
  tname,
  idname,
  data,
  setup_pte_fun,
  subset_fun,
  attgt_fun,
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
  weightsname = NULL,
  gt_type = "att",
  ret_quantile = NULL,
  global_fun = FALSE,
  time_period_fun = FALSE,
  group_fun = FALSE,
  process_dtt_gt_fun = process_dtt_gt,
  process_dose_gt_fun = process_dose_gt,
  biters = 100,
  cl = 1,
  call = NULL,
  ...
)
}
\arguments{
\item{yname}{Name of outcome in \code{data}}

\item{gname}{Name of group in \code{data}}

\item{tname}{Name of time period in \code{data}}

\item{idname}{Name of id in \code{data}}

\item{data}{balanced panel data}

\item{setup_pte_fun}{This is a function that should take in \code{data},
\code{yname} (the name of the outcome variable in \code{data}),
\code{gname} (the name of the group variable),
\code{idname} (the name of the id variable),
and possibly other arguments such as the significance level \code{alp},
the number of bootstrap iterations \code{biters}, and how many clusters
for parallel computing in the bootstrap \code{cl}.  The key thing that
needs to be figured out in this function is which groups and time periods
ATT(g,t) should be computed in.  The function should
return a \code{pte_params} object which contains all of the parameters
passed into the function as well as \code{glist} and \code{tlist} which
should be ordered lists of groups and time periods for ATT(g,t) to be computed.

This function provides also provides a good place for error handling related
to the types of data that can be handled.

The \code{pte} package contains the function \code{setup_pte} that is
a lightweight function that basically just takes the data, omits
the never-treated group from \code{glist} but includes all other groups
and drops the first time period.  This works in cases where ATT would
be identified in the 2x2 case (i.e., where there are two time periods,
no units are treated in the first period and the identification strategy
"works" with access to a treated and untreated group and untreated
potential outcomes for both groups in the first period) --- for example,
this approach works if DID is the identification strategy.}

\item{subset_fun}{This is a function that should take in \code{data},
\code{g} (for group), \code{tp} (for time period), and \code{...}
and be able to return the appropriate \code{data.frame} that can be used
by \code{attgt_fun} to produce ATT(g=g,t=tp).  The data frame should
be constructed using \code{gt_data_frame} in order to guarantee that
it has the appropriate columns that identify which group an observation
belongs to, etc.}

\item{attgt_fun}{This is a function that should work in the case where
there is a single group and the "right" number of time periods to
recover an estimate of the ATT.  For example, in the contest of
difference in differences, it would need to work for a single group,
find the appropriate comparison group (untreated units), find the right
time periods (pre- and post-treatment), and then recover an estimate
of ATT for that group.  It will be called over and over separately
by groups and by time periods to compute ATT(g,t)'s.

The function needs to work in a very specific way.  It should take in the
arguments: \code{data}, \code{...}.  \code{data} should be constructed
using the function \code{gt_data_frame} which checks to make sure
that \code{data} has the correct columns defined.
\code{...} are additional arguments (such as
formulas for covariates) that \code{attgt_fun} needs.  From these arguments
\code{attgt_fun} must return a list with element \code{ATT} containing the
group-time average treatment effect for that group and that time period.

If \code{attgt_fun} returns an influence function (which should be provided
in a list element named \code{inf_func}), then the code will use the
multiplier bootstrap to compute standard errors for group-time average
treatment effects, an overall treatment effect parameter, and a dynamic
treatment effect parameter (i.e., event study parameter).  If
\code{attgt_fun} does not return an influence function, then the same
objects will be computed using the empirical bootstrap.  This is usually
(perhaps substantially) easier to code, but also will usually be (perhaps
substantially) computationally slower.}

\item{cband}{whether or not to report a uniform (instead of pointwise)
confidence band (default is TRUE)}

\item{alp}{significance level; default is 0.05}

\item{boot_type}{should be one of "multiplier" (the default) or "empirical".
The multiplier bootstrap is generally much faster, but \code{attgt_fun} needs
to provide an expression for the influence function (which could be challenging
to figure out).  If no influence function is provided, then the \code{pte}
package will use the empirical bootstrap no matter what the value of this
parameter.}

\item{weightsname}{The name of the column that contains sampling weights.
The default is NULL, in which case no sampling weights are used.}

\item{gt_type}{which type of group-time effects are computed.
The default is "att".  Different estimation strategies can implement
their own choices for \code{gt_type}}

\item{ret_quantile}{For functions that compute quantile treatment effects,
this is a specific quantile at which to report results, e.g.,
\code{ret_quantile = 0.5} will return that the qte at the median.}

\item{global_fun}{Logical indicating whether or not untreated potential
outcomes can be estimated in one shot, i.e., for all groups and time periods.
Main use case would be for one-shot imputation estimators.  Not supported yet.}

\item{time_period_fun}{Logical indicating whether or not untreated potential
outcomes can be estimated for all groups in the same time period.  Not supported yet.}

\item{group_fun}{Logical indicating whether or not untreated potential outcomes
can be estimated for all time periods for a single group.  Not supported yet.
These functions aim at reducing or eliminating running the same code multiple times.}

\item{process_dtt_gt_fun}{An optional function to customize results when
the gt-specific function returns the distribution of treated and untreated
potential outcomes.  The default is \code{process_dtt_gt}, which is a function
provided by the package.  See that function for an example of what this function
should return.  This is unused is unused except in cases where
the results involve distributions.}

\item{process_dose_gt_fun}{An optional function to customize results when the gt-specific
function returns treatment effects that depend on dose (i.e., amount of the
treatment).  The default is \code{process_dose_gt}, which is a function provided
by the package.  See that function for an example of what this function should
return.  This is unused except in cases where the results involve doses.}

\item{biters}{number of bootstrap iterations; default is 100}

\item{cl}{number of clusters to be used when bootstrapping; default is 1}

\item{call}{keeps track of through the \code{call} from external functions/packages}

\item{...}{extra arguments that can be passed to create the correct subsets
of the data (depending on \code{subset_fun}), to estimate group time
average treatment effects (depending on \code{attgt_fun}), or to
aggregating treatment effects (particularly useful are \code{min_e},
\code{max_e}, and \code{balance_e} arguments to event study aggregations)}
}
\value{
\code{pte_results} object
}
\description{
Tools for estimating treatment effects with panel data.

Main function for computing panel treatment effects
}
\examples{
# example using minimum wage data
# and difference-in-differences identification strategy
library(did)
data(mpdta)
did_res <- pte(
  yname = "lemp",
  gname = "first.treat",
  tname = "year",
  idname = "countyreal",
  data = mpdta,
  setup_pte_fun = setup_pte,
  subset_fun = two_by_two_subset,
  attgt_fun = did_attgt,
  xformla = ~lpop
)

summary(did_res)
ggpte(did_res)

}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/bcallaway11/ptetools}
  \item Report bugs at \url{https://github.com/bcallaway11/ptetools/issues}
}

}
\author{
\strong{Maintainer}: Brantly Callaway \email{brantly.callaway@uga.edu}

}
