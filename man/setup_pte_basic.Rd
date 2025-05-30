% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pte_params.R
\name{setup_pte_basic}
\alias{setup_pte_basic}
\title{Basic Setup Function}
\usage{
setup_pte_basic(
  yname,
  gname,
  tname,
  idname,
  data,
  cband = TRUE,
  alp = 0.05,
  boot_type = "multiplier",
  gt_type = "att",
  ret_quantile = 0.5,
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

\item{cband}{whether or not to report a uniform (instead of pointwise)
confidence band (default is TRUE)}

\item{alp}{significance level; default is 0.05}

\item{boot_type}{which type of bootstrap to use}

\item{gt_type}{which type of group-time effects are computed.
The default is "att".  Different estimation strategies can implement
their own choices for \code{gt_type}}

\item{ret_quantile}{For functions that compute quantile treatment effects,
this is a specific quantile at which to report results, e.g.,
\code{ret_quantile = 0.5} will return that the qte at the median.}

\item{biters}{number of bootstrap iterations; default is 100}

\item{cl}{number of clusters to be used when bootstrapping; default is 1}

\item{call}{keeps track of through the \code{call} from external functions/packages}

\item{...}{additional arguments}
}
\value{
\code{pte_params} object
}
\description{
This is a lightweight (example) function for how to setup
the data to be used in the \code{ptetools} package.

\code{setup_pte_basic} takes in information about the structure of \code{data}
and returns a \code{pte_params} object.  The key piece of information
that is computed by this function is the list of groups and list of
time periods where ATT(g,t) should be computed.  In particular, this function
omits the never-treated group but includes all other groups and drops the first
time period.  This setup is basically geared towards the 2x2 case ---
i.e., where ATT could be identified with two periods, a treated and
untreated group, and the first period being pre-treatment for both groups.
This is the relevant case for DID, but is also relevant for other cases as well.
However, for example, if more pre-treatment periods were needed, then this
function should be replaced by something else.

For code that is written with the idea of being easy-to-use by other
researchers, this is a good place to do some error handling / checking
that the data is in the correct format, etc.
}
