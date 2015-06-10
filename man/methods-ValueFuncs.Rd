\name{qFuncs}
\alias{qFuncs}
\alias{qFuncs,DynTxRegime-method}

\title{Q-functions for All Treatment Options}

\description{Returns the estimated Q-functions for each treatment options}

\usage{ \S4method{qFuncs}{DynTxRegime}(object, ...)  }

\arguments{
  \item{object }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\value{
For IQ-Learning and Q-Learning, returns the value 
of the Q-functions for each treatment option.
Not defined for methods optimalSeq and optimalClass.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

