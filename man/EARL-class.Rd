\name{EARL-class}
\Rdversion{1.1}
\docType{class}
\alias{EARL-class}


\title{Class \code{"EARL"}}
\description{
Efficient Augmentation and Relaxation Learning with 
Augmented Inverse Propensity Weighted Estimator (AIPWE) or
Inverse Propensity Weighted Estimator (IPWE)
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EARL", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{optim}:}{Object of class \code{"EARLOptim."} 
      The optimization result. }

    \item{\code{crossValidation}:}{Object of class \code{"CVInfoOrNULL."} 
      The cross validation result. }

    \item{\code{regime}:}{Object of class \code{"formula."} 
      A formula object describing the decision rule. }

    \item{\code{txInfo}:}{Object of class \code{"TxInfoNoSubsets."} 
      The treatment information. }

    \item{\code{decisionFunc}:}{Object of class \code{"numeric."} 
     The estimated decision function. }
  }
}

\section{Extends}{
Class \code{"\link{DynTxRegime-class}"}, directly.
}

\section{Methods}{
  \describe{

    \item{cvInfo}{\code{signature(object = "EARL")}: 
      Retrieve cross validation results. }

    \item{optimObj}{\code{signature(object = "EARL")}: 
      Retrieve optimization results. }

    \item{optTx}{\code{signature(x = "EARL", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. }

    \item{optTx}{\code{signature(x = "EARL", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. }

    \item{print}{\code{signature(x = "EARL")}: 
      Print key results of method. }

    \item{regimeCoef}{\code{signature(object = "EARL")}: 
      Retrieve regime/decision function parameter estimates. }

    \item{show}{\code{signature(object = "EARL")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "EARL")}: 
      Retrieve key summary information of method. }

   }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("EARL")
}

\keyword{internal}

