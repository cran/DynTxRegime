\name{PropensityOnly-class}
\docType{class}
\alias{PropensityOnly-class}

\title{Class \code{"PropensityOnly"}}

\description{
Identifies method as using only propensity score regression.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PropensityOnly", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{propen}:}{An object of class \code{"PropensityRegression."}
     Results of propensity score regression step(s). }
  }
}
\section{Methods}{
\describe{
    \item{coef}{\code{signature(object = "PropensityOnly")}: 
      Retrieve the parameter estimates for each regression. }

    \item{fitObject}{\code{signature(object = "PropensityOnly")}: 
      Retrieve the value object returned by each regression method. }

    \item{outcome}{\code{signature(object = "PropensityOnly")}: 
      Retrieve the value object returned by each regression method  (returns NA). }

    \item{plot}{\code{signature(x = "PropensityOnly")}: 
      Generate plots of each regression object. }

    \item{print}{\code{signature(x = "PropensityOnly")}: 
      Print each regression object. }

    \item{propen}{\code{signature(object = "PropensityOnly")}: 
      Retrieve the value object returned by each propensity
      regression method. }

    \item{show}{\code{signature(object = "PropensityOnly")}: 
      Show key results of each regression object. }

    \item{summary}{\code{signature(object = "PropensityOnly")}: 
      Retrieve key summary information from each regression object. }
}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("PropensityOnly")
}

\keyword{internal}

