\name{SubsetListFit-class}
\docType{class}
\alias{SubsetListFit-class}

\title{Class \code{"SubsetListFit"}}

\description{
Outcome Regression step for a single decision point when subset modeling
is used.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SubsetListFit", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Methods}{
  \describe{
    \item{.predictAllOutcome}{\code{signature(object = "SubsetListFit", data = "data.frame", ...)}: 
    Predict outcome for all subsets at all feasible treatment options.}
      }
}

\section{Extends}{
Classes \code{"\link{SubsetList-class}"}, \code{"\link{OutcomeRegression-class}"},
\code{"\link{SubsetsModeled-class}"}, directly.
Classes \code{"\link{SingleDecisionPoint-class}"} and \code{"\link{List-class}"} by class \code{"\link{SubsetList-class}"}, distance 2.
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("SubsetListFit")
}

\keyword{internal}
