\name{OptimalClassIPWE-class}
\docType{class}
\alias{OptimalClassIPWE-class}

\title{Class \code{"OptimalClassIPWE"}}
\description{
Results from classification method with IPWE.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OptimalClassIPWE", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{classif}:}{An object of class \code{"modelObjFit."}
      The classification analysis. }
  }
}
\section{Extends}{
Classes \code{"\link{OptimalClass-class}"}
and \code{"\link{PropensityOnly-class}"} directly.

Class \code{"\link{DynTxRegime-class},"} by class 
\code{"\link{OptimalClass-class},"} distance 2.
}
\section{Methods}{
  \describe{

    \item{classif}{\code{signature(object = "OptimalClassIPWE")}: 
      Retrieve classification value object. }

    \item{DTRstep}{\code{signature(object = "OptimalClassIPWE")}: 
      Retrieve description of method used to create object. }

    \item{fitObject}{\code{signature(object = "OptimalClassIPWE")}: 
      Retrieve the value object returned by outcome and
      propensity score regression methods. }

    \item{optTx}{\code{signature(x = "OptimalClassIPWE", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. }

    \item{optTx}{\code{signature(x = "OptimalClassIPWE", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. }

    \item{plot}{\code{signature(x = "OptimalClassIPWE")}: 
      Generate plots of the outcome and/or propensity score regression objects. }

    \item{print}{\code{signature(x = "OptimalClassIPWE")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "OptimalClassIPWE")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "OptimalClassIPWE")}: 
      Retrieve key summary information of method. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("OptimalClassIPWE")
}

\keyword{internal}

