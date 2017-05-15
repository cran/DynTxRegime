\name{propen}
\docType{methods}
\alias{propen}

\title{ Retrieve Regression Objects for Propensity Score}
\description{
Retrieve the value object(s) returned by the propensity score
regression method(s).
}

\usage{
propen(object,...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}.}
\item{...}{Ignored.}
}

\value{
The exact structure of the returned list will depend on the
statistical method.

If multiple decision points, the list will have an element for
each decision point named with 'dp=x' where x takes the value of the
decision point. For example, [['dp=1']],
[['dp=2']], etc.

Subset modeling will also result in sub-lists, the names of which
are 'Subset=x' where x takes the values of the subset names as defined
by the user in the input.

}
\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


