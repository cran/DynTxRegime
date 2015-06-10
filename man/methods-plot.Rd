\name{plot}
\alias{plot}
\alias{plot,DynTxRegime-method}


\title{ Generate Standard Plots}
\description{
Generate standard plots. This method uses the \code{plot} function as
defined by the regression method used to obtain parameter estimates.
}
\usage{ \S4method{plot}{DynTxRegime}(x, y, ...)  }

\arguments{
  \item{x }{an object of class DynTxRegime.}
  \item{y }{ignored.}

  \item{\dots }{ignored.}

}

\details{
Titles and/or subtitles will automatically be appended with a character
string indicating the regression model being plotted. This additional
information can be suppressed using an optional argument "suppress = TRUE"
in the call to plot
}

\author{
 Shannon T. Holloway <sthollow@ncsu.edu>
}


