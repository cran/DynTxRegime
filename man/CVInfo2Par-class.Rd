\name{CVInfo2Par-class}
\docType{class}
\alias{CVInfo2Par-class}

\title{Class \code{"CVInfo2Par"}}

\description{
Cross-Validation with two parameters.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CVInfo2Par", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{pars2}:}{An object of class \code{"numeric."} 
                        Second Parameter values considered. }

    \item{\code{opt2}:}{An object of class \code{"numeric."} 
                       Optimal second parameter value. }
  }
}

\section{Extends}{
Class \code{"\link{CVInfo-class}"} directly.
}

\section{Methods}{
  \describe{
    \item{.getPars}{\code{signature(object = "CVInfo2Par")}: 
      Retrieve parameter values considered.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("CVInfo2Par")
}

\keyword{internal}
