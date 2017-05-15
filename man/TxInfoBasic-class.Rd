\name{TxInfoBasic-class}
\docType{class}
\alias{TxInfoBasic-class}

\title{Class \code{"TxInfoBasic"}}

\description{
Stores basic treatment information for a single decision point.
}

\section{Objects from the Class}{
A virtual Class: No object may be created from it.
}

\section{Slots}{
  \describe{

    \item{\code{superSet}:}{An object of class \code{"ANY."} 
                            A vector of all treatment options. }

    \item{\code{txName}:}{An object of class \code{"character."} 
                          The treatment variable name. }
  }
}

\section{Extends}{
Class \code{"\link{SingleDecisionPoint-class}"}, directly.
}

\section{Methods}{
  \describe{
    \item{.getSuperSet}{\code{signature(object = "TxInfoBasic")}: 
      Retrieve super set of treatment options. Method is not exported.}

    \item{.getTxName}{\code{signature(object = "TxInfoBasic")}: 
      Retrieve treatment variable name. Method is not exported.}

    \item{.validTx}{\code{signature(object = "TxInfoBasic", txVec = "ANY")}: 
      Ensure that all elements of \code{txVec} are present in the superset of treatment
      options. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "modelObj", moCont = "modelObj", }\cr
      \code{txInfo = "TxInfoBasic",...)}: 
      Perform outcome regression using either iterative or simple algorithm when
      both main effecta and contrast components are given and subsets are not identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "modelObj", moCont = "NULL", }\cr
      \code{txInfo = "TxInfoBasic", ...)}: 
      Perform outcome regression using simple algorithm when
      only main effects component is given and subsets are not identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "NULL", moCont = "modelObj", } \cr
\code{txInfo = "TxInfoBasic", ...)}: 
      Perform outcome regression using simple algorithm when
      only contrasts component is given and subsets are not identified for
      modeling. Method is not exported.}
  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoBasic")
}

\keyword{internal}
