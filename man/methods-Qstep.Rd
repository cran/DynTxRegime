\name{DTRstep}
\alias{DTRstep}
\alias{DTRstep,DynTxRegime-method}

\title{ Step of an DynTxRegime method}
\description{
 Retrieves the step of the DynTxRegime method contained in a DynTxRegime object.
}
\usage{ \S4method{DTRstep}{DynTxRegime}(object, ...)  }

\arguments{
  \item{object }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\value{
For IQ-Learning, returns a character string identifying the step of the 
IQ-Learning algorithm contained in the DynTxRegime object. For
Q-Learning, returns the step of the Q-Learning algorithm 
contained in the DynTxRegime object. For optimalSeq and optimalClass, 
returns the function name.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

