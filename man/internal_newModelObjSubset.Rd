\name{.newModelObjSubset}
\docType{methods}
\alias{.newModelObjSubset}
\alias{.newModelObjSubset,list-method}

\title{ Create an object of class \code{"ModelObj_SubsetList"}
or \code{"ModelObj_SubsetList_DecisionPointList"}}

\description{
Given a list of objects of class \code{"modelObjSubset"}, 
create a list storing the objects according to decision point.
Method is not exported.
}

\usage{ 
.newModelObjSubset(object)
}

\arguments{
 \item{object}{An object of class \code{"list"}, each element
               an object of class \code{"ModelObjSubset"}.}
}

\details{
When subset modeling is used, users provide methods of DynTxRegime
a list of objects created by calls to \code{buildModelObjSubset()}.
This method determines if that list provides subset models
for a single decision point (stored as an object of class 
\code{"ModelObj_SubsetList"} or for multiple decision points
(subset models are grouped according to their decision point
and stored as an object of class \code{"ModelObj_SubsetList_DecisionPointList."}
}

\value{
  If \code{object} contains subset models for only a single
  decision point, returns an object of class \code{"ModelObj_SubsetList"}.

  If \code{object} contains subset models for multiple
  decision points, returns an object of class \code{"ModelObj_SubsetList_DecisionPointList"}.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

  models <- list()

  models[[1L]] <- buildModelObjSubset(model = ~x1, 
                                      solver.method = 'lm', 
                                      subset = "subset1", 
                                      dp = 1L)

  models[[2L]] <- buildModelObjSubset(model = ~x2, 
                                      solver.method = 'lm', 
                                      subset = "subset2", 
                                      dp = 1L)

  result <- DynTxRegime:::.newModelObjSubset(models)

  is(result)

  models[[3L]] <- buildModelObjSubset(model = ~x3,
                                      solver.method = 'lm', 
                                      subset = "subset1", 
                                      dp = 2L)

  result <- DynTxRegime:::.newModelObjSubset(models)

  is(result)

}

\keyword{internal}
