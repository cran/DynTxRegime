\name{CVInfo-class}
\docType{class}
\alias{CVInfo-class}
\alias{cvInfo,CVInfo-method}

\title{Class \code{"CVInfo"}}

\description{
Cross-Validation parameters and results.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CVInfo", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{value}:}{An object of class \code{"matrix."} 
                         Matrix of values for each parameter
                         considered. }

    \item{\code{pars}:}{An object of class \code{"numeric."} 
                        Parameter values considered. }

    \item{\code{opt}:}{An object of class \code{"numeric."} 
                       Optimal parameter value. }

  }
}

\section{Methods}{
  \describe{
    \item{.getPars}{\code{signature(object = "CVInfo")}: 
      Retrieve parameter values considered.}

    \item{.getValue}{\code{signature(object = "CVInfo")}: 
      Retrieve values at each parameter considered.}

    \item{cvInfo}{\code{signature(object = "CVInfo")}:
      Retrieve cross-validation matrix.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("CVInfo")
}

\keyword{internal}
