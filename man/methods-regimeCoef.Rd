\name{regimeCoef}
\alias{regimeCoef}
\alias{regimeCoef,DynTxRegime-method}

\title{ Retrieve Regime Parameter Estimates}
\description{
 Retrieves treatment regime parameter estimates from an object returned by optimalSeq.
}
\usage{ \S4method{regimeCoef}{DynTxRegime}(object, ...)  }

\arguments{
  \item{object }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\value{
Retrieves the parameter estimates for the treatment rules as determined
by the genetic algorithm of \pkg{rgenoud}.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


