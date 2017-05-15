\name{OptimKernel-class}
\Rdversion{1.1}
\docType{class}
\alias{OptimKernel-class}
\alias{optimObj,OptimKernel-method}
\alias{print,OptimKernel-method}
\alias{show,OptimKernel-method}
\alias{summary,OptimKernel-method}

\title{Class \code{"OptimKernel"}}
\description{
Basic optimization object for weighted learning methods when
kernels are used.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OptimKernel", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{covariates}:}{Object of class \code{"matrix."} 
      The training data covariates matrix for kernel. }

    \item{\code{kernel}:}{Object of class \code{"character."} 
      The description of kernel used. }

    \item{\code{kParam}:}{Object of class \code{"numeric."} 
      The kernel parameter used. }
  }
}

\section{Extends}{
Class \code{"\link{OptimBasic-class}"} directly.
}

\section{Methods}{
  \describe{

    \item{optimObj}{\code{signature(object = "OptimKernel")}: 
      The object returned by optimization routine. }

    \item{.predictOptimalTx}{\code{signature(x = "OptimKernel", newdata = "matrix")}: 
      Predict optimal treatment for new data. }

    \item{.predictOptimalTx}{\code{signature(x = "OptimKernel", newdata = "data.frame")}: 
      Predict optimal treatment for new data. }

    \item{print}{\code{signature(x = "OptimKernel")}: 
      Print key results of optimization. }

    \item{show}{\code{signature(object = "OptimKernel")}: 
      Show key results of optimization. }

    \item{summary}{\code{signature(object = "OptimKernel")}: 
      Return summary information about optimization. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("OptimKernel")
}

\keyword{internal}
