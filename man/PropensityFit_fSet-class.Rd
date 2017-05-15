\name{PropensityFit_fSet-class}
\docType{class}
\alias{PropensityFit_fSet-class}
\alias{predict,PropensityFit_fSet-method}

\title{Class \code{"PropensityFit_fSet"}}

\description{
Propensity score regresssion when subsets are specified
but not used in propensity regression.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PropensityFit_fSet", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{txInfo}:}{Object of class \code{"TxInfoWithSubsets."} 
      The treatment information. }
  }
}
\section{Methods}{
  \describe{

    \item{predict}{\code{signature(object = "PropensityFit_fSet")}: 
      predict propensity for newdata or training data. }
}

}

\section{Extends}{
Class \code{"\link{PropensityFit-class}"} directly.

Classes \code{"modelObjFit"}, \code{"\link{SingleDecisionPoint-class}"}, 
\code{"\link{PropensityRegression-class}"}, and \code{"\link{SubsetsNotModeled-class}"} 
by \code{"\link{PropensityFit-class}"} distance 2.
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("PropensityFit_fSet")
}

\keyword{internal}
