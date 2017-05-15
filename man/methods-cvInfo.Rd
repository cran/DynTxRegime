\name{cvInfo}
\docType{methods}
\alias{cvInfo}

\title{ Retrieve Cross-Validation Results}
\description{
Retrieve cross-validaton results. 
}

\usage{
cvInfo(object)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}
originating from a weighted learning method or EARL.}
}

\value{ For objects returned by owl(), bowl(), rwl(), and earl(),
a matrix of values at each parameter. For all other \code{"DynTxRegime"} objects, NA.}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

