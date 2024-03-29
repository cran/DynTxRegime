% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A_List.R, R/A_DecisionPointList.R,
%   R/A_SubsetList.R
\name{.cycleList}
\alias{.cycleList}
\alias{.cycleList,List-method}
\alias{.cycleList,DecisionPointList-method}
\alias{.cycleList,SubsetList-method}
\title{apply() for \code{List} objects}
\usage{
.cycleList(object, ...)

\S4method{.cycleList}{List}(object, func, trm = "object", nm = NULL, ...)

\S4method{.cycleList}{DecisionPointList}(object, func, trm = "object", nm = "dp=", ...)

\S4method{.cycleList}{SubsetList}(object, func, trm = "object", nm = "Subset=", ...)
}
\arguments{
\item{object}{The object inheriting from list to which func is applied.}

\item{...}{Additional arguments to be passed to func.}

\item{func}{A character. The name of the function to be called for
each element of object.}

\item{trm}{A character. The formal input argument name through
which each element of object is passed to func.}

\item{nm}{A character. The naming convention for element of the
returned list or displayed in print/show calls.}
}
\value{
If func returns a value object, a list containing the
  value objects returned by func.
}
\description{
Applies the specified function to each element of the \code{List}.
}
\keyword{internal}
