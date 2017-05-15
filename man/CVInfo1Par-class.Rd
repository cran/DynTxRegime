\name{CVInfo1Par-class}
\docType{class}
\alias{CVInfo1Par-class}

\title{Class \code{"CVInfo1Par"}}

\description{
Cross-Validation with one parameter.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CVInfo1Par", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Extends}{
Class \code{"\link{CVInfo-class}"} directly.
}

\section{Methods}{
  \describe{
    \item{.getPars}{\code{signature(object = "CVInfo1Par")}: 
      Retrieve parameter values considered.}

    \item{.getValue}{\code{signature(object = "CVInfo1Par")}: 
      Retrieve values at each parameter considered.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("CVInfo1Par")
}

\keyword{internal}
