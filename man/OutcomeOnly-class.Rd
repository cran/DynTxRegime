\name{OutcomeOnly-class}
\docType{class}
\alias{OutcomeOnly-class}

\title{Class \code{"OutcomeOnly"}}

\description{
Identifies method as using only outcome regression.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OutcomeOnly", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{outcome}:}{An object of class \code{"OutcomeRegression."}
     Results of outcome regression step(s). }
  }
}
\section{Methods}{
\describe{
    \item{.predictAllOutcome}{\code{signature(object = "OutcomeOnly", data = "data.frame")}: 
    Predict outcome for all treatment options.
      }

    \item{coef}{\code{signature(object = "OutcomeOnly")}: 
      Retrieve the parameter estimates for each regression. }

    \item{fitObject}{\code{signature(object = "OutcomeOnly")}: 
      Retrieve the value object returned by each regression method. }

    \item{outcome}{\code{signature(object = "OutcomeOnly")}: 
      Retrieve the value object returned by each regression method. }

    \item{plot}{\code{signature(x = "OutcomeOnly")}: 
      Generate plots of each regression object. }

    \item{print}{\code{signature(x = "OutcomeOnly")}: 
      Print each regression object. }

    \item{propen}{\code{signature(object = "OutcomeOnly")}: 
      Retrieve the value object returned by each propensity
      regression method (returns NA). }

    \item{show}{\code{signature(object = "OutcomeOnly")}: 
      Show key results of each regression object. }

    \item{summary}{\code{signature(object = "OutcomeOnly")}: 
      Retrieve key summary information from each regression object. }
}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("OutcomeOnly")
}

\keyword{internal}

