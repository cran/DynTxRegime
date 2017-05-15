\name{sd}
\alias{sd}
\alias{sd,IQLearnFS_C-method}

\title{Standard Deviation of IQ-Learning Variance Step}

\description{Returns the standard deviation of the
the residuals of the IQ-Learning contrast regression (IQ3).}

\usage{
sd(x, na.rm = FALSE)
\S4method{sd}{IQLearnFS_C}(x, na.rm = FALSE)
}

\arguments{
\item{x}{An object of class \code{"IQLearnFS_C"}.}
\item{na.rm}{TRUE if NA values are to be removed.}
}

\value{
For objects returned by iqLearnFSC(), a numeric object. For
all other \code{"DynTxRegime"} objects, NA. 
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

