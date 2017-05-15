\name{EARLIPWE-class}
\docType{class}
\alias{EARLIPWE-class}

\title{Class \code{"EARLIPWE"}}
\description{
Efficient Augmentation and Relaxation Learning with 
Inverse Propensity Weighted Estimator}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EARLIPWE", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Extends}{
Classes \code{"\link{EARL-class}"} and
\code{"\link{PropensityOnly-class}"} directly.

Class \code{"\link{DynTxRegime-class}"} by class \code{"\link{EARL-class}"},
distance 2.
}

\section{Methods}{
  \describe{

    \item{DTRstep}{\code{signature(object = "EARLIPWE")}: 
      Retrieve descripton of method. }

    \item{print}{\code{signature(x = "EARLIPWE")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "EARLIPWE")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "EARLIPWE")}: 
      Retrieve key summary information of method. }

   }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("EARLIPWE")
}

\keyword{internal}

