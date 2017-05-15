\name{qqplot}
\docType{methods}
\alias{qqplot}
\alias{qqplot,IQLearnFS_VHet-method}


\title{ IQ-Learning: Generate QQ-Plots for Variance Modeling .}
\description{
Generate QQ plots for Variance modeling in IQ-Learning.
}

\usage{
qqplot(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
                 ylab = deparse(substitute(y)),...)
\S4method{qqplot}{IQLearnFS_VHet}(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
                 ylab = deparse(substitute(y)),...)
}


\arguments{
\item{x}{An object of class \code{"IQLearnFS_VHet"}.}
\item{y}{Ignored.}
\item{plot.it}{logical. Should the result be plotted?}
\item{xlab}{Plot labels.}
\item{ylab}{Plot labels.}
\item{...}{Graphical parameters.}
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

