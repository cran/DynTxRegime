\name{RWLOptim-class}
\Rdversion{1.1}
\docType{class}
\alias{RWLOptim-class}
\alias{optimObj,RWLOptim-method}
\alias{print,RWLOptim-method}
\alias{residuals,RWLOptim-method}
\alias{show,RWLOptim-method}
\alias{summary,RWLOptim-method}


\title{Class \code{"RWLOptim"}}
\description{
Optimization routines for Residual Wweighted Learning method.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RWLOptim", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{beta}:}{An object of class \code{"numeric."} 
      The estimated derivatives of concave object function }
    \item{\code{residuals}:}{An object of class \code{"numeric."} 
      The residuals of outcome regression. }
  }
}

\section{Extends}{
Class \code{"\link{OptimKernel-class}"} directly.
Class \code{"\link{OptimBasic-class}"} by \code{"\link{OptimKernel-class}"}, distance 2.
}

\section{Methods}{
  \describe{
    \item{optimObj}{\code{signature(object = "RWLOptim")}: 
      Retrieve parameter estimates. }
    \item{print}{\code{signature(x = "RWLOptim")}: 
      Print key results of optimization. }
    \item{residuals}{\code{signature(object = "RWLOptim")}: 
      Retrieve residuals of outcome regression }
    \item{show}{\code{signature(object = "RWLOptim")}: 
      Show key results of optimization. }
    \item{summary}{\code{signature(object = "RWLOptim")}: 
      Return summary information about optimization. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("RWLOptim")
}

\keyword{internal}
