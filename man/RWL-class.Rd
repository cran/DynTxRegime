\name{RWL-class}
\docType{class}
\alias{RWL-class}

\title{Class \code{"RWL"}}
\description{
Implementation of Residual Weighted Learning
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RWL", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{regime}:}{An object of class \code{"formula."} 
      The formula description of covariates included in kernel. }

    \item{\code{crossValidation}:}{An object of class \code{"CVInfoOrNULL."}
      The cross validation results. }

    \item{\code{optim}:}{An object of class \code{"RWLOptim."} 
      The optimization results. }

    \item{\code{txInfo}:}{An object of class \code{"TxInfo."} 
      The treatment information. }

    \item{\code{responseType}:}{An object of class \code{"character."} 
      The type of response. }
  }
}
\section{Extends}{
Classes \code{"\link{DynTxRegime-class}"} and
\code{"\link{PropensityAndOutcome-class}"} directly.
}

\section{Methods}{
  \describe{
    \item{DTRstep}{\code{signature(object = "RWL")}: 
      Retrieve description of method used to create object. }
    \item{cvInfo}{\code{signature(object = "RWL")}: 
      Retrieve cross-validation results. }
    \item{optTx}{\code{signature(x = "RWL", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata }
    \item{optTx}{\code{signature(x = "RWL", newdata = "missing")}: 
      Retrieve estimated optimal treatment for newdata }
    \item{optimObj}{\code{signature(object = "RWL")}: 
      Retrieve optimization results. }
    \item{print}{\code{signature(x = "RWL")}: 
      Print key results of method. }
    \item{regimeCoef}{\code{signature(object = "RWL")}: 
      Retrieve regime parameter estimates. }
    \item{residuals}{\code{signature(object = "RWL")}: 
      Retrieve the residuals of the outcome regression. }
    \item{show}{\code{signature(object = "RWL")}: 
      Show key results of method. }
    \item{summary}{\code{signature(object = "RWL")}: 
      Retrieve key summary information of method. }
  }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("RWL")
}

\keyword{internal}

