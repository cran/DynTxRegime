\name{IQLearnFS_C-class}
\Rdversion{1.1}
\docType{class}
\alias{IQLearnFS_C-class}

\title{Class \code{"IQLearnFS_C"}}
\description{
Results of the regression of the estimated contrasts of the 
outcome of interest from IQ-Learning algorithm (IQ3).
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IQLearnFS_C", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{

    \item{\code{txVec}:}{An object of class \code{"numeric."}
       First-stage treatments in training data. }

    \item{\code{qFunc}:}{An object of class \code{"matrix."}
       The fitted values at both treatment options. }

    \item{\code{residuals}:}{An object of class \code{"numeric."}
      Residuals of the regression analysis.}

    \item{\code{yContHatSS}:}{An object of class \code{"numeric."}
      The estimated contrasts component of the outcome of interest.}
  }
}
\section{Extends}{
Classes  \code{"\link{IQLearnBase-class}"},
\code{"\link{IQLearnFS-class}"}, and
\code{"\link{DynTxRegime-class}"} directly.

Class \code{"\link{OutcomeOnly-class}"} by class \code{"\link{IQLearnBase-class}"}, distance 2.
}
\section{Methods}{
  \describe{

    \item{DTRstep}{\code{signature(object = "IQLearnFS_C")}: 
      Retrieve description of method implemented to obtain object. }

    \item{print}{\code{signature(x = "IQLearnFS_C")}: 
      Print key results of method. }

    \item{residuals}{\code{signature(x = "IQLearnFS_C")}: 
      Retrieve residuals of the regression. }

    \item{sd}{\code{signature(x = "IQLearnFS_C")}: 
      Standard deviation of residuals of the regression. }

    \item{show}{\code{signature(object = "IQLearnFS_C")}: 
      Show key results of method. }

    \item{stdResiduals}{\code{signature(x = "IQLearnFS_C")}: 
      Standardized residuals of the regression. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("IQLearnFS_C")
}
\keyword{internal}

