\name{BOWLWithSubsetRegimes-class}
\docType{class}
\alias{BOWLWithSubsetRegimes-class}
\alias{optimObj,BOWLWithSubsetRegimes-method}
\alias{regimeCoef,BOWLWithSubsetRegimes-method}
\alias{cvInfo,BOWLWithSubsetRegimes-method}

\title{Class \code{"BOWLWithSubsetRegimes"}}

\description{
A BOWL step when subset modeling is used for decision rules.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BOWLWithSubsetRegimes", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Extends}{
Classes \code{"\link{SubsetList-class}"} and \code{"\link{BOWLObj-class}"} directly.
}

\section{Methods}{
  \describe{

    \item{cvInfo}{\code{signature(object = "BOWLWithSubsetRegimes")}: 
      Retrieve cross-validation results. }

    \item{optimObj}{\code{signature(object = "BOWLWithSubsetRegimes")}: 
      Retrieve optimization results. }

    \item{.predictOptimalTx}{\code{signature(x = "BOWLBinarySubsetRegimes", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. 
      Method is not exported.}
    \item{.predictOptimalTx}{\code{signature(x = "BOWLBinarySubsetRegimes", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. Method is not exported.}

    \item{regimeCoef}{\code{signature(object = "BOWLWithSubsetRegimes")}: 
      Retrieve regime parameter estimates. }
  }
}

\author{ Shannon T. Holloway <sthollow@ncsu.edu> }

\examples{
showClass("BOWLWithSubsetRegimes")
}

\keyword{internal}
