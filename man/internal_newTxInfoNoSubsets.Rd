\name{.newTxInfoNoSubsets}
\docType{methods}
\alias{.newTxInfoNoSubsets}
\alias{.newTxInfoNoSubsets,character,factor-method}
\alias{.newTxInfoNoSubsets,character,integer-method}

\title{ Create Treatment Information Object When Subsets are not Identified}

\description{
Creates an object of class \code{"\link{TxInfoNoSubsets-class}"}.
Method is not exported.
}

\usage{
.newTxInfoNoSubsets(txName, txVec, ...)
\S4method{.newTxInfoNoSubsets}{character,factor}(txName, txVec, suppress)
\S4method{.newTxInfoNoSubsets}{character,integer}(txName, txVec, suppress)
}

\arguments{
 \item{txName}{The column header of \code{"data.frame"} indicating the
               treatment variable.}
 \item{txVec }{The treatment variable.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{suppress}{T/F indicating if screen prints are generated.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

# Integer treatment 
txVec <- rbinom(100,1,0.5)
txName <- "A1"

tst <- DynTxRegime:::.newTxInfoNoSubsets(txName = txName, 
                                         txVec = txVec,  
                                         suppress = FALSE)

is(tst)

# Factor treatment
txVec <- as.factor(txVec)

tst <- DynTxRegime:::.newTxInfoNoSubsets(txName = txName,  
                                         txVec = txVec,  
                                         suppress = FALSE)

is(tst)

}

\keyword{internal}
