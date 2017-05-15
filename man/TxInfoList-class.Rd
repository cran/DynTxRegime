\name{TxInfoList-class}
\docType{class}
\alias{TxInfoList-class}

\title{Class \code{"TxInfoList"}}

\description{
A list of \code{"TxInfoBasic"} objects for multiple-decision-point
analyses.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TxInfoList", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Extends}{
Classes \code{"\link{List-class}"} and \code{"\link{MultipleDecisionPoint-class}"}, directly.
}

\section{Methods}{
  \describe{
    \item{.newOutcomeRegression}{\code{signature(moMain = "ModelObj_DecisionPointList", } \cr
\code{moCont = "ModelObj_DecisionPointList", txInfo = "TxInfoList", ...)}: 
      Perform outcome regression using either iterative or simple algorithm when
      both main effects and contrast components are given and subsets are not identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "ModelObj_DecisionPointList", } \cr
\code{moCont = "NULL", txInfo = "TxInfoList", ...)}: 
      Perform outcome regression using simple algorithm when
      only main effects components are given and subsets are not identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "NULL", } \cr
\code{moCont = "ModelObj_DecisionPointList", txInfo = "TxInfoList", ...)}: 
      Perform outcome regression using simple algorithm when
      only contrast components are given and subsets are not identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "ModelObj_SubsetList_DecisionPointList", } \cr
\code{moCont = "ModelObj_SubsetList_DecisionPointList", txInfo = "TxInfoList", ...)}: 
      Perform outcome regression using either iterative or simple algorithm when
      both main effects and contrast components are given and subsets are identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "ModelObj_SubsetList_DecisionPointList", } \cr
\code{moCont = "NULL", txInfo = "TxInfoList", ...)}: 
      Perform outcome regression using simple algorithm when
      only main effects components are given and subsets are identified for
      modeling. Method is not exported.}

    \item{.newOutcomeRegression}{\code{signature(moMain = "NULL", } \cr
\code{moCont = "ModelObj_SubsetList_DecisionPointList", txInfo = "TxInfoList", ...)}: 
      Perform outcome regression using simple algorithm when
      only contrast components are given and subsets are identified for
      modeling. Method is not exported.}
  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxInfoList")
}

\keyword{internal}

