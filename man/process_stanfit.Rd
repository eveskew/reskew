% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_stanfit.R
\name{process_stanfit}
\alias{process_stanfit}
\title{Process a fit Stan model}
\usage{
process_stanfit(fit.stan.model, n.pars.to.trim)
}
\arguments{
\item{fit.stan.model}{A Stan model object.}

\item{n.pars.to.trim}{The number of parameters in the Stan model to \strong{exclude} from calculations. Useful for excluding \code{lp__} and any parameters that are saved at every model iteration, thus becoming unwieldy.}
}
\description{
This function processes a fit Stan model object, recording divergent iterations, Rhat values, and, if they have been stored, log-likelihood values along with WAIC.
}
\examples{
process_stanfit(my.stan.model, 1)
}
