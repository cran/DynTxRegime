\name{PropensityAndOutcome-class}
\docType{class}
\alias{PropensityAndOutcome-class}

\title{Class \code{"PropensityAndOutcome"}}

\description{
Identifies method as using both propensity score and outcome regressions.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PropensityAndOutcome", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{propen}:}{An object of class \code{"PropensityRegression."}
     Results of propensity score regression step(s). }
    \item{\code{outcome}:}{An object of class \code{"OutcomeRegression."}
     Results of outcome regression step(s). }
  }
}
\section{Methods}{
\describe{
    \item{coef}{\code{signature(object = "PropensityAndOutcome")}: 
      Retrieve the parameter estimates for each regression. }

    \item{fitObject}{\code{signature(object = "PropensityAndOutcome")}: 
      Retrieve the value object returned by each regression method. }

    \item{outcome}{\code{signature(object = "PropensityAndOutcome")}: 
      Retrieve the value object returned by each regression method. }

    \item{plot}{\code{signature(x = "PropensityAndOutcome")}: 
      Generate plots of each regression object. }

    \item{print}{\code{signature(x = "PropensityAndOutcome")}: 
      Print each regression object. }

    \item{propen}{\code{signature(object = "PropensityAndOutcome")}: 
      Retrieve the value object returned by each propensity
      regression method. }

    \item{show}{\code{signature(object = "PropensityAndOutcome")}: 
      Show key results of each regression object. }

    \item{summary}{\code{signature(object = "PropensityAndOutcome")}: 
      Retrieve key summary information from each regression object. }
}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("PropensityAndOutcome")
}

\keyword{internal}

