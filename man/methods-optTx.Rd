\name{optTx}
\docType{methods}
\alias{optTx}
\alias{optTx,DynTxRegime,missing-method}
\alias{optTx,DynTxRegime,data.frame-method}
\alias{optTx,IQLearnFS,missing-method}
\alias{optTx,IQLearnFS,data.frame-method}

\title{Methods to Retrieve Estimated  Optimal Treatment
       or to Predict Optimal Treatment}

\description{
Retrieves the estimated or predicted optimal treatment.
}

\usage{
optTx(x, newdata, ...)
\S4method{optTx}{DynTxRegime,data.frame}(x, newdata)
\S4method{optTx}{DynTxRegime,missing}(x, newdata)
\S4method{optTx}{IQLearnFS,data.frame}(x, newdata, ..., y = NULL, z = NULL, dens = NULL)
\S4method{optTx}{IQLearnFS,missing}(x, newdata, ..., y = NULL, z = NULL, dens = NULL)
}

\arguments{
\item{x}{An object of class \code{"DynTxRegime"}.}
\item{newdata}{\code{"data.frame"} of covariates}
\item{...}{Ignored. Used to required named input}
\item{y}{An object of class \code{IQLearnFS}.}
\item{z}{An object of class \code{IQLearnFS}.}
\item{dens}{\code{"character"} indicating density estimator. One
of \{'nonpar','norm'\}.}
}


\value{
       For outcome regression methods Q-Learning and IQ-Learning 
       and weighted learning methods OWL, BOWL, RWL, and EARL, a list
       with elements: 'decisionFunc', a matrix of estimated Q-Functions
       and 'optimalTx', a vector of estimated optimal treatments.
       For value-search methods optimalClass() and optimalSeq() a 
       matrix of estimated optimal treatments.
}

\author{Shannon T. Holloway <sthollow@ncsu.edu>}
