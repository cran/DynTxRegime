\name{.newCVInfo}
\docType{methods}
\alias{.newCVInfo}
\alias{.newCVInfo,numeric,missing-method}
\alias{.newCVInfo,numeric,numeric-method}

\title{ Create Cross-Validation Objects}

\description{
Given parameter options, determine optimal parameter value.
Method is not exported
}

\usage{
.newCVInfo(lambdas, kparam, ...)
\S4method{.newCVInfo}{numeric,missing}(optimFunc, valueFunc, response, prWgt, txVec, cvFolds, lambdas, x, 
           suppress, ...)
\S4method{.newCVInfo}{numeric,numeric}(optimFunc, valueFunc, response, prWgt, txVec, cvFolds, lambdas, x,  
           kernel, kparam, suppress, ...)
}

\arguments{
 \item{optimFunc}{\code{"character"} name of function to be used to perform optimization.}
 \item{valueFunc}{\code{"character"} name of function to be used to estimate value.}
 \item{response}{Outcome of interest.}
 \item{prWgt}{Propensity weighting vector.}
 \item{txVec}{Treatment vector coded as +1/-1.}
 \item{cvFolds}{Number of cross-validation folds to complete.}
 \item{lambdas}{Vector of tuning parameters.}
 \item{x}{\code{"matrix"} of covariates and treatment histories.}
 \item{suppress}{T/F indicating of screen prints are generated.}
 \item{kernel}{\code{"character"} description of kernel to be used.}
 \item{kparam}{NULL or kernel parameter(s) to consider.}
 \item{...}{Used to pass additional arguments to \code{optimFunc}.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\keyword{internal}

