\name{IQLearnSS-class}
\docType{class}
\alias{IQLearnSS-class}

\title{Class \code{"IQLearnSS"}}
\description{
Results of the second-stage IQ-Learning algorithm.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IQLearnSS", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{yContHat}:}{Object of class \code{"numeric."}
      Contrast component of fitted outcome. }
 
    \item{\code{yMainHat}:}{Object of class \code{"numeric."}
      Main effects component of fitted outcome. }

    \item{\code{delta}:}{Object of class \code{"numeric."}
      Indicator of compliance with optimal treatment regime. }

    \item{\code{decisionFunc}:}{Object of class \code{"matrix."}
      Estimated outcome of interest at each treatment option. }
  }
}
\section{Extends}{
Classes \code{"\link{IQLearnBase-class}"} and \code{"\linkS4class{DynTxRegime},"} directly.

Class \code{"\link{OutcomeOnly-class}"} by class \code{"\link{IQLearnBase-class}"}, distance 2.


}
\section{Methods}{
  \describe{

    \item{DTRstep}{\code{signature(object = "IQLearnSS")}: 
      Retrieve description of method implemented to obtain object. }

    \item{fittedCont}{\code{signature(object = "IQLearnSS")}: 
      Retrieve the estimated contrast component of the outcome. }

    \item{fittedMain}{\code{signature(object = "IQLearnSS")}: 
      Retrieve the estimated main effects component of the outcome. }

    \item{optTx}{\code{signature(x = "IQLearnSS", newdata = "data.frame")}: 
      Predict optimal treatments for newdata. }

    \item{optTx}{\code{signature(x = "IQLearnSS", newdata = "missing")}: 
      Retrieve the optimal treatments for training data. }

    \item{print}{\code{signature(x = "IQLearnSS")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "IQLearnSS")}: 
      Show key results of method. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("IQLearnSS")
}

\keyword{internal}

