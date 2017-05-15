\name{BOWLBasic-class}
\Rdversion{1.1}
\docType{class}
\alias{BOWLBasic-class}
\alias{cvInfo,BOWLBasic-method}
\alias{optimObj,BOWLBasic-method}
\alias{regimeCoef,BOWLBasic-method}
\alias{print,BOWLBasic-method}
\alias{show,BOWLBasic-method}
\alias{summary,BOWLBasic-method}

\title{Class \code{"BOWLBasic"}}

\description{
Results for a single step of the BOWL algorithm for
a subset of data with binary treatment options and no
subsets for propensity modeling. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BOWLBasic", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{optTx}:}{Object of class \code{"numeric."}
                         Estimated optimal treatment for training data. 
                         Coded as +1.0/-1.0 independent of 
                         notation provided in original data.}

    \item{\code{estVal}:}{Object of class \code{"numeric."}
                         Estimated value of regime for training data. }

    \item{\code{regime}:}{Object of class \code{"formula."}
                         Formula description of covariates used in 
                         kernel. }

    \item{\code{crossValidation}:}{Object of class \code{"CVInfoOrNULL."}
                         Cross-validation results for training data. }

    \item{\code{optim}:}{Object of class \code{"OWLOptim."}
                         Optimization results. }

    \item{\code{decisionFunc}:}{Object of class \code{"numeric."}
                         Estimated decision function for origina data. }
  }
}

\section{Methods}{
  \describe{

    \item{cvInfo}{\code{signature(object = "BOWLBasic")}:
      Retrieve cross-validation matrix.}
    \item{optimObj}{\code{signature(object = "BOWLBasic")}: 
      Retrieve optimization results. }
    \item{.predictOptimalTx}{\code{signature(x = "BOWLBasic", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. 
      Method is not exported.}
    \item{.predictOptimalTx}{\code{signature(x = "BOWLBasic", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. Method is not exported.}
    \item{print}{\code{signature(x = "BOWLBasic")}: 
      Print key results of method. }
    \item{regimeCoef}{\code{signature(object = "BOWLBasic")}: 
      Retrieve parameter estimates for decision function. }
    \item{show}{\code{signature(object = "BOWLBasic")}: 
      Show key results of method. }
    \item{summary}{\code{signature(object = "BOWLBasic")}: 
      Retrieve key summary information of method. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("BOWLBasic")
}

\keyword{internal}
