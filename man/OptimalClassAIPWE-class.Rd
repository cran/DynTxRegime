\name{OptimalClassAIPWE-class}
\docType{class}
\alias{OptimalClassAIPWE-class}

\title{Class \code{"OptimalClassAIPWE"}}
\description{
Results from classification method with AIPWE.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OptimalClassAIPWE", ...)}.
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
and \code{"\link{PropensityAndOutcome-class}"} directly.

Class \code{"\link{DynTxRegime-class},"} by class 
\code{"\link{OptimalClass-class},"} distance 2.
}
\section{Methods}{
  \describe{

    \item{classif}{\code{signature(object = "OptimalClassAIPWE")}: 
      Retrieve classification value object. }

    \item{DTRstep}{\code{signature(object = "OptimalClassAIPWE")}: 
      Retrieve description of method used to create object. }

    \item{fitObject}{\code{signature(object = "OptimalClassAIPWE")}: 
      Retrieve the value object returned by outcome and
      propensity score regression methods. }

    \item{optTx}{\code{signature(x = "OptimalClassAIPWE", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. }

    \item{optTx}{\code{signature(x = "OptimalClassAIPWE", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. }

    \item{plot}{\code{signature(x = "OptimalClassAIPWE")}: 
      Generate plots of the outcome and/or propensity score regression objects. }

    \item{print}{\code{signature(x = "OptimalClassAIPWE")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "OptimalClassAIPWE")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "OptimalClassAIPWE")}: 
      Retrieve key summary information of method. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("OptimalClassAIPWE")
}

\keyword{internal}

