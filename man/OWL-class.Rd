\name{OWL-class}
\Rdversion{1.1}
\docType{class}
\alias{OWL-class}

\title{Class \code{"OWL"}}
\description{
Implementation of Outcome Weighted Learning.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OWL", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{shift}:}{An object of class \code{"numeric."} 
      The amount reward shifted to make all positive. }

    \item{\code{modelFormula}:}{An object of class \code{"formula."} 
      The formula description of kernel covariates. }

    \item{\code{crossValidation}:}{An object of class \code{"CVInfo."} 
      The results of cross validation }

    \item{\code{optim}:}{An object of class \code{"OWLOptim."} 
      The OWLOptim object }

    \item{\code{txInfo}:}{An object of class \code{"TxInfo."} 
      The treatment information }

    \item{\code{decisionFunc}:}{An object of class \code{"numeric."} 
      Estimated value of the decision function for training data. }
  }
}
\section{Extends}{
Classes \code{"\link{PropensityOnly-class}"} and \code{"\link{DynTxRegime-class}"} directly.
}
\section{Methods}{
  \describe{
    \item{cvInfo}{\code{signature(object = "OWL")}:
      Retrieve cross-validation matrix.}
    \item{DTRstep}{\code{signature(object = "OWL")}: 
      Retrieve description of method used to create object. }
    \item{optimObj}{\code{signature(object = "OWL")}: 
      Retrieve optimization results. }
    \item{optTx}{\code{signature(x = "OWL", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. }
    \item{print}{\code{signature(x = "OWL")}: 
      Print key results of method. }
    \item{regimeCoef}{\code{signature(object = "OWL")}: 
      Retrieve regime parameter estimates. }
    \item{show}{\code{signature(object = "OWL")}: 
      Show key results of method. }
    \item{summary}{\code{signature(object = "OWL")}: 
      Retrieve key summary information of method. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("OWL")
}

\keyword{internal}

