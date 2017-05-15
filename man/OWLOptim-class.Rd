\name{OWLOptim-class}
\docType{class}
\alias{OWLOptim-class}
\alias{optimObj,OWLOptim-method}
\alias{summary,OWLOptim-method}

\title{Class \code{"OWLOptim"}}
\description{
Optimization object for Outcome Weighted Learning.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OWLOptim", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Extends}{
Class \code{"\link{OptimKernel-class}"} directly.
Class \code{"\link{OptimBasic-class}"} by \code{"\link{OptimKernel-class}"}, distance 2.
}
\section{Methods}{
  \describe{
    \item{optimObj}{\code{signature(object = "OWLOptim")}: 
      The object returned by optimization routine. }
    \item{summary}{\code{signature(object = "OWLOptim")}: 
      Return summary information about optimization. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("OWLOptim")
}
\keyword{internal}
