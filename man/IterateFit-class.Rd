\name{IterateFit-class}
\docType{class}
\alias{IterateFit-class}
\alias{coef,IterateFit-method}
\alias{fitObject,IterateFit-method}
\alias{plot,IterateFit,ANY-method}
\alias{predict,IterateFit-method}
\alias{print,IterateFit-method}
\alias{show,IterateFit-method}
\alias{summary,IterateFit-method}

\title{Class \code{"IterateFit"}}

\description{
Results from an iterative fit for an outcome regression step.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IterateFit", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{fitObjME}:}{An object of class \code{"TypedSimpleFit."}
     Regression analysis for main effects component. }

    \item{\code{fitObjC}:}{An object of class \code{"TypedSimpleFit."} 
      Regression analysis for contrasts component. }

  }
}
\section{Extends}{
Classes \code{"\link{SingleDecisionPoint-class}"}, 
\code{"\link{OutcomeRegression-class}"}, and \code{"\link{SubsetsNotModeled-class}"} directly.
}
\section{Methods}{
\describe{
    \item{.predictAllOutcome}{\code{signature(object = "IterateFit", data = "data.frame")}: 
    Predict outcome for all treatment options.
      }

    \item{coef}{\code{signature(object = "IterateFit")}: 
      Retrieve the parameter estimates for each regression. }

    \item{fitObject}{\code{signature(object = "IterateFit")}: 
      Retrieve the value object returned by each regression method. }

    \item{plot}{\code{signature(x = "IterateFit")}: 
      Generate plots of each regression object. }

    \item{predict}{\code{signature(object = "IterateFit")}: 
      Predict outcome for new data or training data. }

    \item{print}{\code{signature(x = "IterateFit")}: 
      Print each regression object. }

    \item{show}{\code{signature(object = "IterateFit")}: 
      Show key results of each regression object. }

    \item{summary}{\code{signature(object = "TypedSimpleFit")}: 
      Retrieve key summary information from each regression object. }
}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("IterateFit")
}

\keyword{internal}

