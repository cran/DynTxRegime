\name{estimator}
\docType{methods}
\alias{estimator}
\alias{estimator,DynTxRegime-method}
\alias{estimator,IQLearnFS-method}

\title{ Estimated Value of Estimated Optimal Regime}

\description{
Retrieve the estimated value of the estimated optimal treatment regime
for the training data.
}

\usage{
estimator(x,...)
\S4method{estimator}{DynTxRegime}(x)
\S4method{estimator}{IQLearnFS}(x, w = NULL, y = NULL, z = NULL, dens = NULL)
}

\arguments{
 \item{x}{Object of class \code{"DynTxRegime"}.}
 \item{...}{Ignored.}
 \item{w}{Object of class \code{"IQLearnFS"} or \code{"IQLearnSS"}.}
 \item{y}{Object of class \code{"IQLearnFS"} or \code{"IQLearnSS"}.}
 \item{z}{Object of class \code{"IQLearnFS"} or \code{"IQLearnSS"}.}
 \item{dens}{\code{"character"} indicating density approximation. Must
             be one of \{'nonpar', 'norm'\}.}
}

\details{

For the IQ-learning algorithm, additional inputs 
are required to obtain the estimated value of the
first-stage treatment. Function estimator() required 
inputs \code{w}, \code{x}, \code{y}, and \code{z} are the three first-stage 
and single second-stage objects (in no specific order). 

For all other methods, no information beyond the value object returned
by the method is needed.

}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

