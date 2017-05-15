\name{plot}
\alias{plot}
\docType{methods}
\alias{plot}
\alias{plot,DynTxRegime-method}

\title{ Generate Standard Plots}
\description{
Generate standard plots. This method uses the \code{plot} function as
defined by the regression method used to obtain parameter estimates.
}

\usage{
plot(x, y, ...)
\S4method{plot}{DynTxRegime}(x, suppress = FALSE, ...)
}

\arguments{
\item{x}{An object of class \code{"DynTxRegime"}.}
\item{y}{Ignored.}
\item{suppress}{Logical indicating if identifiers should be added to titles.}
\item{...}{Additional arguments passed to plot()}
}

\details{
Titles and/or subtitles will automatically be appended with a character
string indicating the regression model being plotted. This additional
information can be suppressed using optional argument "suppress = TRUE"
in the call to plot
}

\author{
 Shannon T. Holloway <sthollow@ncsu.edu>
}


