\name{TypedSimpleFitWithSubsets-class}
\docType{class}
\alias{TypedSimpleFitWithSubsets-class}
\alias{predict,TypedSimpleFitWithSubsets-method}

\title{Class \code{"TypedSimpleFitWithSubsets"}}

\description{
An outcome regression step with a single model and no
subset modeling is used for outcome regression but is identified
for other purposes.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TypedSimpleFitWithSubsets", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{txInfo}:}{An object of class \code{"TxInfoWithSubsets."} 
                          The treatment information. }

  }
}

\section{Methods}{
  \describe{
    \item{.predictAllOutcome}{\code{signature(object = "TypedSimpleFitWithSubsets", data = "data.frame", ...)}: 
    Predict outcome for all subsets at all feasible treatment options.
      }
    \item{predict}{\code{signature(object = "TypedSimpleFitWithSubsets", newdata = "data.frame", ...)}: 
    Predict outcome for data provided.}

    \item{predict}{\code{signature(object = "TypedSimpleFitWithSubsets", newdata = "missing", ...)}: 
    Predict outcome for oriiginal training data.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TypedSimpleFitWithSubsets")
}

\keyword{internal}

