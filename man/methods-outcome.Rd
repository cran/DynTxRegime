\name{outcome}
\docType{methods}
\alias{outcome}

\title{ Retrieve Regression Objects for the Outcome Regression Models}

\description{
Retrieve the value object(s) returned by the outcome regression method(s).
}

\usage{
outcome(object,...)
}

\arguments{
\item{object}{An object of class \code{"DynTxRegime"}.}
\item{...}{Ignored.}
}

\value{
A list. See details section for further information.
}

\details{
The exact structure of the returned object will depend on the
statistical method.

If outcome regression models were fit in a single step,
the list returned will be one of \{'Combined', 'moMain', 'moCont'\}
indicating that the main effects and contrast models were combined,
that only a main effects model was provided, or that only
a contrast model was provided. If the iterative
algorithm was used, the list will have two elements: 'moMain' and 'moCont.'

If the method included multiple decision points, the list will 
have an element for
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


