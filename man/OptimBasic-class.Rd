\name{OptimBasic-class}
\docType{class}
\alias{OptimBasic-class}
\alias{optimObj,OptimBasic-method}
\alias{print,OptimBasic-method}
\alias{regimeCoef,OptimBasic-method}
\alias{show,OptimBasic-method}
\alias{summary,OptimBasic-method}

\title{Class \code{"OptimBasic"}}
\description{
Basic optimization object for weighted learning methods.
}
\section{Objects from the Class}{
A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{threshold}:}{Object of class \code{"numeric."} 
      The estimated threshold }

    \item{\code{alpha}:}{Object of class \code{"numeric."} 
      Parameter estimates. }

    \item{\code{lambda}:}{Object of class \code{"numeric."} 
      The tuning parameter used. }

    \item{\code{optim}:}{Object of class \code{"ANY."} 
      The object returned by optimization method }
  }
}
\section{Methods}{
  \describe{

    \item{optimObj}{\code{signature(object = "OptimBasic")}: 
      The object returned by optimization routine. }

    \item{print}{\code{signature(x = "OptimBasic")}: 
      Print key results of optimization. }

    \item{regimeCoef}{\code{signature(object = "OptimBasic")}: 
      Retrieve parameter estimates. }

    \item{show}{\code{signature(object = "OptimBasic")}: 
      Show key results of optimization. }

    \item{summary}{\code{signature(object = "OptimBasic")}: 
      Return summary information about optimization. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("OptimBasic")
}

\keyword{internal}
