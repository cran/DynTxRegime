\name{genetic}
\alias{genetic}
\alias{genetic,DynTxRegime-method}

\title{ Retrieve the Result of the Genetic Algorithm Optimization}

\description{
Retrieve the value objects returned by the rgenoud algorithm.
}
\usage{ \S4method{genetic}{DynTxRegime}(object, ...)  }

\arguments{
  \item{object }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\details{
This method returns NULL for all DynTxRegime objects except those
returned by optimalSeq().
}

\value{
       See ?genoud for details.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


