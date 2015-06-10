\name{qqPlot}
\alias{qqPlot}
\alias{qqPlot,DynTxRegime-method}


\title{ IQ-Learning: Generate QQ-Plots for Variance Modeling .}
\description{
Generate QQ plots for Variance modeling in IQ-Learning.
}
\usage{ \S4method{qqPlot}{DynTxRegime}(x, ...)  }

\arguments{
  \item{x }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\details{
Generates a qq-plot for the variance modeling step of the IQ-Learning
algorithm. This plot is used to determine if the normal or empirical
density estimator is appropriate. If the observations deviate from the
line, den='nonpar' should be used in the final IQ-Learning step.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

