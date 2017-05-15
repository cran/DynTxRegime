\name{TxInfoNoSubsets-class}
\docType{class}
\alias{TxInfoNoSubsets-class}

\title{Class \code{"TxInfoNoSubsets"}}

\description{
Identifies treatment information objects as not pertaining to subsets.
}

\section{Extends}{
Class \code{"\link{TxInfoBasic-class}"}, directly.
Class \code{"\link{SingleDecisionPoint-class}"} by class \code{"\link{TxInfoBasic-class}"}, distance 2.
}

\section{Methods}{
  \describe{
    \item{.newTypedSimpleFit}{\code{signature(moMain = "modelObj", moCont = "modelObj", } \cr
      \code{txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression when no subset modeling is specified and both
      main effects and contrast components are given for model. Method is not exported.}

    \item{.newTypedSimpleFit}{\code{signature(moMain = "modelObj", moCont = "NULL", } \cr
      \code{txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression when no subset modeling is specified and 
      only main effects component is given for model. Method is not exported.}

    \item{.newTypedSimpleFit}{\code{signature(moMain = "NULL", moCont = "modelObj", } \cr
      \code{txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression when no subset modeling is specified and only
      contrasts component is given for model. Method is not exported.}

    \item{.newIterateFit}{\code{signature(moMain = "modelObj", moCont = "modelObj", } \cr
      \code{txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression using iterative algorithm when no subset modeling is specified and both
      main effects and contrast components are given for model. Method is not exported.}

    \item{.newPropensityRegression}{\code{signature(moPropen = "modelObj", txInfo = "TxInfoNoSubsets", ...)}: 
      Perform propensity score regression when no subset modeling is specified. Method is not exported.}

  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoNoSubsets")
}

\keyword{internal}
