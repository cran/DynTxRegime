% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/P_class_.owl.R
\docType{class}
\name{internal-owl-class}
\alias{internal-owl-class}
\title{Class \code{.owl}}
\description{
Class \code{.owl} stores parameters required for OWL optimization step
}
\section{Slots}{

\describe{
\item{\code{x}}{Matrix of covariates for kernel}

\item{\code{txSignR}}{Vector of tx multiplied by the sign of the response}

\item{\code{txVec}}{Vector of tx coded as -1/1}

\item{\code{absRinvPi}}{Vector of the absolute value of the response weighted by
the propensity for the tx received}

\item{\code{response}}{Vector of the response}

\item{\code{invPi}}{Vector of the inverse of the propensity for the tx received}

\item{\code{surrogate}}{The Surrogate for the loss-function}

\item{\code{pars}}{Vector of regime parameters}

\item{\code{kernel}}{The Kernel defining the decision function}
}}

\keyword{internal}
