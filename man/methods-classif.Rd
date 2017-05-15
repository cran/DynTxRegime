\name{classif}
\docType{methods}
\alias{classif}

\title{ Retrieve Classification Value Object}

\description{
Retrieve the value object returned by the classification method
used to estimate an optimal treatment regime when viewed as a weighted
classification problem, i.e., method optimalClass().
}

\usage{
classif(object, ...)
}

\arguments{

 \item{object}{An object of class \code{"DynTxRegime"}.}

 \item{...}{ignored.}

}

\details{
The exact value and structure of the returned 
object is determined by the method specified for input \code{moClass}
of optimalClass().
Because the value object of the modeling function is returned, 
any methods developed for said object can be applied to the
object returned. For example, if the
classification method is \code{rpart}, the object returned
by classif() can be passed as input to residuals.rpart(),
path.rpart(), etc. without modification.
}

\value{
For objects returned by optimalClass(), see Details section.
For all other \code{"DynTxRegime"} objects, NA.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


