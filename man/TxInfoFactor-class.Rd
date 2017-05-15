\name{TxInfoFactor-class}
\docType{class}
\alias{TxInfoFactor-class}

\title{Class \code{"TxInfoFactor"}}

\description{
Stores basic treatment information for a single decision point
when treatment is of class \code{"factor."}
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TxInfoFactor", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{superSet}:}{An object of class \code{"character."} 
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
    \item{.compare}{\code{signature(object = "TxInfoFactor", vec1 = "factor", vec2 = "factor")}: 
      Compare \code{vec1} and \code{vec2} element-by_element for equality.
      Method is not exported.}

    \item{.convertTx}{\code{signature(object = "TxInfoFactor", txVec = "ANY")}: 
      Convert \code{txVec} to a factor vector.
      Method is not exported.}

    \item{.getLevels}{\code{signature(object = "TxInfoFactor", txVec = "factor")}: 
      Determine levels of factor vector input \code{txVec}.
      Method is not exported.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoFactor")

obj <- new("TxInfoFactor", superSet = c("A","B"), txName = "A1")

txVec1 <- as.factor(c("A","B","A","B"))
txVec2 <- as.factor(c("A","B","A","A"))

DynTxRegime:::.compare(object = obj, vec1 = txVec1, vec2 = txVec2)

DynTxRegime:::.convertTx(object = obj, txVec = c("A","B","A","B"))

DynTxRegime:::.getLevels(object = obj, txVec = txVec1)

DynTxRegime:::.getSuperSet(object = obj)

DynTxRegime:::.getTxName(object = obj)

DynTxRegime:::.validTx(object = obj, txVec = c("A","B"))

try(DynTxRegime:::.validTx(object = obj, txVec = c("A","C")))

}

\keyword{internal}
