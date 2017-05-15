\name{residuals}
\docType{methods}
\alias{residuals}

\title{ Extract Model Residuals}
\description{
For methods that employ residuals, retrieve residuals.
}

\usage{
residuals(object,...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}.}
\item{...}{Ignored.}
}

\value{For objects returned by iqLearnFSC(), the residuals of the
contrast component regression. For iqLearnFSV(), the 
standardized fitted residuals. For rwl(),
a vector of the residuals used by the method to obtain
the optimal treatment. For all other \code{"DynTxRegime"} objects, NA.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


