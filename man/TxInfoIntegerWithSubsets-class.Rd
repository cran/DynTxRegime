\name{TxInfoIntegerWithSubsets-class}
\docType{class}
\alias{TxInfoIntegerWithSubsets-class}

\title{Class \code{"TxInfoIntegerWithSubsets"}}

\description{
Stores basic treatment information for a single decision point
when treatment is of class \code{"integer"} and includes subsets.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TxInfoIntegerWithSubsets", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{superSet}:}{An object of class \code{"integer."} 
                            A vector of all treatment options. }

  }
}
\section{Extends}{
Classes \code{"\link{TxSubset-class}"} and \code{"\link{TxInfoWithSubsets-class}"}, directly.
Class \code{"\link{TxInfoBasic-class}"} by class \code{"\link{TxInfoWithSubsets-class}"}, distance 2.
Class \code{"\link{SingleDecisionPoint-class}"} by class \code{"\link{TxInfoBasic-class}"}, distance 3.
}

\section{Methods}{
  \describe{
    \item{.compare}{\code{signature(object = "TxInfoIntegerWithSubsets", vec1 = "integer", vec2 = "integer")}: 
      Integer vectors of treatment options to compare 
      element-by_element for equality. Method is not exporeted.}

    \item{.convertTx}{\code{signature(object = "TxInfoIntegerWithSubsets", txVec = "numeric")}: 
      Convert additional input \code{txVec} to an integer
      vector. Method is not exported.}

    \item{.getLevels}{\code{signature(object = "TxInfoIntegerWithSubsets", txVec = "integer")}: 
      Determine levels of integer vector input \code{txVec}. Method is not exported}

  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoIntegerWithSubsets")

fSet <- function(x1){
          subsets <- list("subset1" = c(0L,1L),
                          "subset2" = c(0L))

          ptsSubset <- character(length(x1))
          ptsSubset[x1 > 0] <- "subset1"
          ptsSubset[x1 <= 0] <- "subset2"
}

obj <- new("TxInfoIntegerWithSubsets",
           superSet = c(0L,1L),
           txName = "A1",
           ptsSubset = c("subset1", "subset2", "subset1", "subset2"),
           subsetRule = fSet,
           subsets = list("subset1" = c(0L,1L),
                          "subset2" = c(0L)),
           singleton = c(FALSE,TRUE,FALSE,TRUE))

txVec1 <- c(0L,1L,0L,1L)
txVec2 <- c(0L,0L,0L,1L)

DynTxRegime:::.compare(object = obj, vec1 = txVec1, vec2 = txVec2)

DynTxRegime:::.convertTx(object = obj, txVec = c(0L,1L,0L))

DynTxRegime:::.getLevels(object = obj, txVec = txVec1)

DynTxRegime:::.getSuperSet(object = obj)

DynTxRegime:::.getTxName(object = obj)

DynTxRegime:::.getPtsSubset(object = obj)

DynTxRegime:::.getSingleton(obj)

DynTxRegime:::.getSubsetRule(obj)

DynTxRegime:::.getSubsets(obj)

DynTxRegime:::.validTx(obj, c(1L,0L,0L,0L))

try(DynTxRegime:::.validTx(obj, c(0L,0L,2L,1L)))

}

\keyword{internal}

