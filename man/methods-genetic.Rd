\name{genetic}
\docType{methods}
\alias{genetic}

\title{ Retrieve the Result of the Genetic Algorithm Optimization}

\description{
Retrieve the value object returned by the rgenoud algorithm.
Only available for objects returned by optimalSeq().
See ?genoud for further information.
}

\usage{
genetic(object, ...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}.}
\item{...}{Ignored.}
}

\value{For objects returned by optimalSeq(),
a list, the structure of which is defined by rgenoud(). For
all other \code{"DynTxRegime"} objects, NA.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


