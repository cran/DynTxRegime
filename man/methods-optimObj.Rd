\name{optimObj}
\docType{methods}
\alias{optimObj}

\title{ Retrieve Optimization Value Object}
\description{
Retrieve the value object returned by the optimization method for 
the weighted learning and EARL statistical methods. 
}

\usage{
optimObj(object, ...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}
originating from a weighted learning method or EARL.}
\item{...}{Ignored.}
}

\value{ For objects returned by owl(), bowl(), rwl(), and earl(), a
list, the exact structure of which depends on the
optimization algorithm used by method. For all other 
\code{"DynTxRegime"} objects, NA. }

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

