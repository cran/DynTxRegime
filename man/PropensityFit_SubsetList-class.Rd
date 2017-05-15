\name{PropensityFit_SubsetList-class}
\docType{class}
\alias{PropensityFit_SubsetList-class}
\alias{predict,PropensityFit_SubsetList-method}

\title{Class \code{"PropensityFit_SubsetList"}}
\description{
Propensity regression when subset modeling is used.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PropensityFit_SubsetList", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{txInfo}:}{An object of class \code{"TxInfoWithSubsets."}
      Treatment information.}
  }
}

\section{Extends}{
Classes \code{"\link{SubsetList-class},"}
\code{"\link{PropensityRegression-class},"} and
\code{"\link{SubsetsModeled-class}"} directly.

Classes 
\code{"\link{SingleDecisionPoint-class}"} and 
\code{"\link{List-class}"} by class \code{"\link{SubsetList-class},"}
distance 2.
}

\section{Methods}{
  \describe{

    \item{predict}{\code{signature(object = "PropensityFit_SubsetList")}: 
      Predict propensity score for all subsets. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("PropensityFit_SubsetList")
}

\keyword{internal}
