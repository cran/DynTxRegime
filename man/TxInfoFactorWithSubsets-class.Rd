\name{TxInfoFactorWithSubsets-class}
\docType{class}
\alias{TxInfoFactorWithSubsets-class}

\title{Class \code{"TxInfoFactorWithSubsets"}}

\description{
Stores basic treatment information for a single decision point
when treatment is of class \code{"factor"} and includes subsets.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TxInfoFactorWithSubsets", ...)}.
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
Classes \code{"\link{TxSubset-class}"} and \code{"\link{TxInfoWithSubsets-class}"}, directly.
Class \code{"\link{TxInfoBasic-class}"} by class \code{"\link{TxInfoWithSubsets-class}"}, distance 2.
Class \code{"\link{SingleDecisionPoint-class}"} by class \code{"\link{TxInfoBasic-class}"}, distance 3.
}

\section{Methods}{
  \describe{
    \item{.compare}{\code{signature(object = "TxInfoFactorWithSubsets", vec1 = "factor", vec2 = "factor")}: 
      Factor vectors of treatment options to compare 
      element-by_element for equality. Method is not exporeted.}

    \item{.convertTx}{\code{signature(object = "TxInfoFactorWithSubsets", txVec = "ANY")}: 
      Convert additional input \code{txVec} to a factor
      vector. Method is not exported.}

    \item{.getLevels}{\code{signature(object = "TxInfoFactorWithSubsets", txVec = "factor")}: 
      Determine levels of factor vector input \code{txVec}. Method is not exported}
  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoFactorWithSubsets")

fSet <- function(x1){
          subsets <- list("subset1" = c("A","B"),
                          "subset2" = c("A"))

          ptsSubset <- character(length(x1))
          ptsSubset[x1 > 0] <- "subset1"
          ptsSubset[x1 <= 0] <- "subset2"
}

obj <- new("TxInfoFactorWithSubsets",
           superSet = c("A","B"),
           txName = "A1",
           ptsSubset = c("subset1", "subset2", "subset1", "subset2"),
           subsetRule = fSet,
           subsets = list("subset1" = c("A","B"),
                          "subset2" = c("A")),
           singleton = c(FALSE,TRUE,FALSE,TRUE))

txVec1 <- as.factor(c("A","B","A","B"))
txVec2 <- as.factor(c("A","A","A","B"))

DynTxRegime:::.compare(object = obj, vec1 = txVec1, vec2 = txVec2)

DynTxRegime:::.convertTx(object = obj, txVec = c("A","B","A"))

DynTxRegime:::.getLevels(object = obj, txVec = txVec1)

DynTxRegime:::.getSuperSet(object = obj)

DynTxRegime:::.getTxName(object = obj)

DynTxRegime:::.getPtsSubset(object = obj)

DynTxRegime:::.getSingleton(object = obj)

DynTxRegime:::.getSubsetRule(object = obj)

DynTxRegime:::.getSubsets(object = obj)

DynTxRegime:::.validTx(object = obj, c("B","A","A","A"))

try(DynTxRegime:::.validTx(object = obj, c("B","B","A","A")))

}

\keyword{internal}
