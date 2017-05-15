\name{BOWLWithOneRegime-class}
\docType{class}
\alias{BOWLWithOneRegime-class}
\alias{cvInfo,BOWLWithOneRegime-method}
\alias{optimObj,BOWLWithOneRegime-method}
\alias{print,BOWLWithOneRegime-method}
\alias{regimeCoef,BOWLWithOneRegime-method}
\alias{show,BOWLWithOneRegime-method}
\alias{summary,BOWLWithOneRegime-method}

\title{Class \code{"BOWLWithOneRegime"}}

\description{
A BOWL step without subset modeling of treatment regime.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BOWLWithOneRegime", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{

    \item{\code{obj}:}{An object of class \code{"BOWLBasic."}}
}}

\section{Extends}{
Class \code{"\link{BOWLObj-class}"} directly.
}

\section{Methods}{
  \describe{

    \item{cvInfo}{\code{signature(object = "BOWLWithOneRegime")}:
      Retrieve cross-validation matrix.}
    \item{optimObj}{\code{signature(object = "BOWLWithOneRegime")}: 
      Retrieve optimization results. }
    \item{.predictOptimalTx}{\code{signature(x = "BOWLWithOneRegime", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. 
      Method is not exported.}
    \item{.predictOptimalTx}{\code{signature(x = "BOWLWithOneRegime", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. Method is not exported.}
    \item{print}{\code{signature(x = "BOWLWithOneRegime")}: 
      Print key results of method. }
    \item{regimeCoef}{\code{signature(object = "BOWLWithOneRegime")}: 
      Retrieve parameter estimates for decision function. }
    \item{show}{\code{signature(object = "BOWLWithOneRegime")}: 
      Show key results of method. }
    \item{summary}{\code{signature(object = "BOWLWithOneRegime")}: 
      Retrieve key summary information of method. }
}
}

\author{ Shannon T. Holloway <sthollow@ncsu.edu> }

\examples{
showClass("BOWLWithOneRegime")
}

\keyword{internal}
