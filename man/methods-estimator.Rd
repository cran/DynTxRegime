\name{estimator}
\alias{estimator}
\alias{estimator,DynTxRegime-method}

\title{ Mean Predicted Outcome }

\description{
 Retrieve the mean predicted outcome if estimated optimal treatment is followed
by all patients in the training data.
}

\usage{ \S4method{estimator}{DynTxRegime}(x, ...)  }

\arguments{
  \item{x     }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\details{

For the first-stage of IQ-learning, all first-stage objects must be
provided as input (arguments x, y, z in no specific order). 
In addition, dens must be one of {norm, nonpar}. Specifically,
the call takes the form estimator(x,y,z,dens).

}

\value{
Returns the mean predicted outcome if the estimated optimal treatment is followed
by all patients in the training data. }

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


