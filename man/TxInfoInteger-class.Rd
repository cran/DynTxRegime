\name{TxInfoInteger-class}
\docType{class}
\alias{TxInfoInteger-class}

\title{Class \code{"TxInfoInteger"}}

\description{
Stores basic treatment information for a single decision point
when treatment is of class \code{"integer."}
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TxInfoInteger", ...)}.
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
Class \code{"\link{TxInfoNoSubsets-class}"}, directly.
Class \code{"\link{TxInfoBasic-class}"} by class \code{"\link{TxInfoNoSubsets-class}"}, distance 2.
Class \code{"\link{SingleDecisionPoint-class}"} by class \code{"\link{TxInfoBasic-class}"}, distance 3.
}


\section{Methods}{
  \describe{
    \item{.compare}{\code{signature(object = "TxInfoFactor", vec1 = "integer", vec2 = "integer")}: 
      Compare \code{vec1} and \code{vec2} element-by_element for equality.
      Method is not exported.}

    \item{.convertTx}{\code{signature(object = "TxInfoFactor", txVec = "numeric")}: 
      Convert \code{txVec} to a factor vector.
      Method is not exported.}

    \item{.getLevels}{\code{signature(object = "TxInfoFactor", txVec = "integer")}: 
      Determine levels of factor vector input \code{txVec}.
      Method is not exported.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoInteger")

obj <- new("TxInfoInteger", superSet = c(0L,1L), txName = "A1")

txVec1 <- c(0L,1L,0L,1L)
txVec2 <- c(0L,1L,0L,0L)

DynTxRegime:::.compare(object = obj, vec1 = txVec1, vec2 = txVec2)

DynTxRegime:::.convertTx(object = obj, txVec = c(0,1,0,1))

DynTxRegime:::.getLevels(object = obj, txVec = txVec1)

DynTxRegime:::.getSuperSet(object = obj)

DynTxRegime:::.getTxName(object = obj)

DynTxRegime:::.validTx(object = obj, txVec = c(0L,1L))

try(DynTxRegime:::.validTx(object = obj, txVec = c(0L,2L)))
}

\keyword{internal}
