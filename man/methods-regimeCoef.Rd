\name{regimeCoef}
\docType{methods}
\alias{regimeCoef}

\title{ Retrieve Regime Parameter Estimates}
\description{
 Retrieves treatment regime parameter estimates.
}

\usage{
regimeCoef(object,...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}.}
\item{...}{Ignored.}
}

\value{ Defined only for objects returned by 
owl(), bowl(), rwl(), earl(), and optimalSeq().
For single-decision-point scenarios, a vector of the 
estimated treatment regime parameters.
For multiple-decision-point scenarioes, a list; each element containing
the estimated treatment regime parameters for the decision point.
For all other \code{"DynTxRegime"} objects, NA.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


