\name{fittedCont}
\docType{methods}
\alias{fittedCont}

\title{ Extract Fitted Contrast Component of Outcome}
\description{
Retrieve the fitted contrast component of the outcome
regression analysis for the second-stage decision point.
Only available for the IQ-Learning algorithm.
}

\usage{
fittedCont(object, ...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}}
\item{...}{Ignored.}
}

\value{ For objects returned by iqLearnSS(), a vector. 
For all other \code{"DynTxRegime"} objects, NA.}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

