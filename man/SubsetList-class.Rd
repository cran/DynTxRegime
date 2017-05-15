\name{SubsetList-class}
\docType{class}
\alias{SubsetList-class}
\alias{coef,SubsetList-method}
\alias{fitObject,SubsetList-method}
\alias{plot,SubsetList,ANY-method}
\alias{print,SubsetList-method}
\alias{show,SubsetList-method}
\alias{summary,SubsetList-method}

\title{Class \code{"SubsetList"}}
\description{
Stores results for subset modeling. Each element hold results
or models for a single subset. The name of the element is the
subset nickname as provided by user.
}
\section{Objects from the Class}{
A virtual Class: No objects may be created from it.
}

\section{Slots}{
  \describe{
    \item{\code{loo}:}{An object of class \code{"list."} 

      A list of subset modeling results or models. }
  }
}

\section{Extends}{
Classes \code{"\link{List-class}"} and 
\code{"\link{SingleDecisionPoint-class}"}, directly.
}

\section{Methods}{
  \describe{

    \item{coef}{\code{signature(object = "SubsetList")}: 
      Retrieve the parameter estimates for each regression. }

    \item{fitObject}{\code{signature(object = "SubsetList")}: 
      Retrieve the value object returned by each regression method. }

    \item{plot}{\code{signature(x = "SubsetList", y  = "ANY")}: 
      Generate plots of each regression object. }

    \item{predict}{\code{signature(object = "SubsetList")}: 
      Obtain predictions for each subset. If subset
      is not available to a patient, NA is returned. }

    \item{print}{\code{signature(x = "SubsetList")}: 
      Print each regression object. }

    \item{show}{\code{signature(object = "SubsetList")}: 
      Show key results of each regression object. }

    \item{summary}{\code{signature(object = "SubsetList")}: 
      Retrieve key summary information from each regression object. }

  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("SubsetList")
}


\keyword{internal}
