\name{fitObject}
\docType{methods}
\alias{fitObject}

\title{ Modeling Function Value Objects}

\description{
Retrieve the value objects returned by the regression method(s).
Extends methods defined in \pkg{modelObj}.
}

\usage{
fitObject(object, ...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}.}
\item{...}{Ignored.}
}

\details{

The exact structure of the returned list will depend on the
statistical method.

For methods that employ outcome regression, the list will have one
element named 'outcome' in which all fit information
for the outcome regression step(s) will be given.

For methods that use propensity score modeling, the list will have one
element named 'propensity' in which all fit information
for the propensity regression step(s) will be given.

The 'outcome' and 'propensity' elements may also be lists. For example,
if multiple decision points, the sub-list will have an element for
each decision point named with 'dp=x' where x takes the value of the
decision point. For example, [['outcome']][['dp=1']],
[['outcome']][['dp=2']], etc.

Subset modeling will also result in sub-lists, the names of which
are 'Subset=x' where x takes the values of the subset names as defined
by the user in the input.


}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

