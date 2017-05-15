\name{ModelObjSubset-class}
\docType{class}
\alias{ModelObjSubset-class}

\title{Class \code{"ModelObjSubset"}}

\description{
Class created by call to \code{buildModelObjSubset()} to specify a 
modelObj for a specific subset and decision point.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ModelObjSubset", ...)}.
These objects are created by the user through buildModelObjSubset()
See ?buildModelObjSubset for details.
}

\section{Slots}{
  \describe{

    \item{\code{decisionPoint}:}{An object of class \code{"integer."} 

      The decision point for which this model is defined. }

    \item{\code{subset}:}{An object of class \code{"character."}
 
      The subset for which this model is defined.
      Note that if more than one subset, the names should be
      collapsed with a comma separator ("a" and "b" = "a,b") }

    \item{\code{modelObj}:}{An object of class \code{"modelObj."}
 
      The model object to be used for the specified subset and 
      decision point. }
  }
}

\section{Methods}{
\describe{

    \item{.getDecisionPoint}{\code{signature(object = "ModelObjSubset")}: 
      Retrieve decision point. Not exported.}

    \item{.getSubset}{\code{signature(object = "ModelObjSubset")}: 
      Retrieve subset nickname. Not exported.}

    \item{.getModelObj}{\code{signature(object = "ModelObjSubset")}: 
      Retrieve modeling object. Not exported.}

}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("ModelObjSubset")

subsetModel <- buildModelObjSubset(model  = ~ x1, 
                                   solver.method = 'lm', 
                                   subset = 'example', 
                                   dp = 1)

DynTxRegime:::.getDecisionPoint(subsetModel)
DynTxRegime:::.getSubset(subsetModel)
DynTxRegime:::.getModelObj(subsetModel)
}

\keyword{internal}
