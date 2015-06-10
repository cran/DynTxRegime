\name{propen}
\alias{propen}
\alias{propen,DynTxRegime-method}


\title{ Retrieve Regression Objects for Propensity for Treatment}
\description{
Retrieve the value object returned by the propensity for treatment
regression methods used in optimalClass and optimalSeq methods.
}
\usage{ \S4method{propen}{DynTxRegime}(object, ...)  }

\arguments{
  \item{object }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\value{
Retrieves the value object returned by the regression method
used to estimate the propensity for treatment. If multiple decision
points or subset models were used, a list of objects is returned,
where the kth element corresponds to the kth model.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


