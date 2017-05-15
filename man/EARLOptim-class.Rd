\name{EARLOptim-class}
\docType{class}
\alias{EARLOptim-class}
\alias{optimObj,EARLOptim-method}

\title{Class \code{"EARLOptim"}}
\description{
Optimization for Efficient Augmentation and Relaxation Learning
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EARLOptim", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{weight}:}{Object of class \code{"matrix."} 
      The weight vectors used in optimization. }

    \item{\code{surrogate}:}{Object of class \code{"character."} 
      The surrogate of 0-1 loss function used. }
  }
}

\section{Extends}{
Class \code{"\link{OptimBasic-class}"} directly.
}

\section{Methods}{
  \describe{

    \item{optimObj}{\code{signature(object = "EARLOptim")}: 
      Retrieve optimization result. }

    \item{.predictOptimalTx}{\code{signature(x = "EARLOptim", newdata = "matrix")}:
      Predict optimal treatment for new data.}

    \item{.predictOptimalTx}{\code{signature(x = "EARLOptim", newdata = "data.frame")}:
      Predict optimal treatment for new data.}
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("EARLOptim")
}

\keyword{internal}
