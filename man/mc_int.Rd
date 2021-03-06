% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mc_int.R
\name{mc_int}
\alias{mc_int}
\title{Simple Monte-Carlo integration}
\usage{
mc_int(x_range, fun, B, seed = 1291)
}
\arguments{
\item{x_range}{A \code{vector} of dimension 2 used to denote the integration
region of interest, i.e. [a, b].}

\item{fun}{A \code{string} containing the function to be integrated. It
is assumed that \code{x} is used as the variable of interest.}

\item{B}{A \code{numeric} (integer) used to denote the number of simulations.}

\item{seed}{A \code{numeric} used to control the seed of the random number
generator used by this function.}
}
\value{
A \code{list} containing the following attributes:
\describe{
     \item{I}{Estimated value of the integral}
     \item{var}{Estimated variance of the estimator}
}
}
\description{
Compute an approximation of the integral of the function f(x)
with respect to dx in the range [a, b] by Monte-Carlo integration using
uniform sampling.
}
\examples{
mc_int(x_range = c(0,1), fun = "x^2", B = 10^5)
mc_int(x_range = c(0,1), fun = "x^2*sin(x^2/pi)", B = 10^5)
}
\author{
Stephane Guerrier
}
