\name{TxInfoWithSubsets-class}
\docType{class}
\alias{TxInfoWithSubsets-class}

\title{Class \code{"TxInfoWithSubsets"}}

\description{
Used to identify treatment information objects as pertaining to subsets.
}

\section{Extends}{
Class \code{"\link{TxInfoBasic-class}"}, directly.
Class \code{"\link{SingleDecisionPoint-class}"} by class \code{"\link{TxInfoBasic-class}"}, distance 2.
}

\section{Methods}{
  \describe{
    \item{.newTypedSimpleFit}{\code{signature(moMain = "modelObj", moCont = "modelObj", } \cr
     \code{txInfo = "TxInfoWithSubsets", ...)}: 
      Perform outcome regression using both main effects and
      contrast components when no subset modeling is specified 
      for outcome regression but subsets are specified for
      other purposes. Method is not exported.}

    \item{.newTypedSimpleFit}{\code{signature(moMain = "modelObj", moCont = "NULL", } \cr
     \code{txInfo = "TxInfoWithSubsets", ...)}: 
      Perform outcome regression using only a main effects component
      when no subset modeling is specified 
      for outcome regression but subsets are specified for other
      purposes. Method is not exported.}

    \item{.newTypedSimpleFit}{\code{signature(moMain = "NULL", moCont = "modelObj", } \cr
     \code{txInfo = "TxInfoWithSubsets", ...)}: 
      Perform outcome regression using only a contrast component
      when no subset modeling is specified 
      for outcome regression but subsets are specified for other
      purposes. Method is not exported.}

    \item{.newIterateFit}{\code{signature(moMain = "modelObj", moCont = "modelObj", } \cr
     \code{txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression using iterative algorithm when no
      subset modeling is specified for the outcome regression
      modeling but subsets are specified for other purposes. 
      Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "ModelObj_SubsetList", } \cr
      \code{moCont = "ModelObj_SubsetList", txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression using either iterative or simple algorithm when
      both main effects and contrast components are given and subsets are identified for
      outcome regression modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "ModelObj_SubsetList", moCont = "NULL",} \cr
      \code{txInfo = "TxInfoNoSubsets")}: 
      Perform outcome regression using simple algorithm when
      only a main effects component is given and subsets are identified for
      outcome regression modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "NULL", moCont = "ModelObj_SubsetList",} \cr
      \code{ txInfo = "TxInfoNoSubsets", ...)}: 
      Perform outcome regression using simple algorithm when
      only a contrasts component is given and subsets are identified for
      outcome regression modeling. Method is not exported.}

}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoWithSubsets")
}

\keyword{internal}

