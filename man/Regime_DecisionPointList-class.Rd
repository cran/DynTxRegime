\name{Regime_DecisionPointList-class}
\docType{class}
\alias{Regime_DecisionPointList-class}
\alias{regimeCoef,Regime_DecisionPointList-method}

\title{Class \code{"Regime_DecisionPointList"}}

\description{
A list of objects of class \code{"Regime"}
 for multiple-decision-point methods.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regime_DecisionPointList", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Extends}{
Classes \code{"\link{RegimeObject-class}"} and
\code{"\link{DecisionPointList-class}"} directly.

Classes \code{"\link{List-class}"} and \code{"\link{MultipleDecisionPoint-class}"}
by class \code{"\link{DecisionPointList-class}"}, distance 2.
}

\section{Methods}{
  \describe{
    \item{.getNumPars}{\code{signature(object = "Regime_DecisionPointList")}: 
      Retrieve a list, ith element contains the number of parameters in the ith decision rule. Method not exported.}

    \item{.getParNames}{\code{signature(object = "Regime_DecisionPointList")}: 
      Retrieve a list, ith element contains the names of the parameters
   for the ith decision rule. Method not exported.}

    \item{.getPars}{\code{signature(object = "Regime_DecisionPointList")}: 
      Retrieve a list, ith element contains the parameter estimates
  for the ith decision rule. Method not exported.}

    \item{.getRegimeFunction}{\code{signature(object = "Regime_DecisionPointList")}: 
      Retrieve a list, ith element contains the decision rule function
   for the ith decision point. Method not exported.}

    \item{.predictOptimalTx}{\code{signature(x = "Regime", newdata = "data.frame", dp="integer")}:
      Estimate the optimal treatment for newdata at dp decision point.
      Method not exported.
     }

    \item{.setPars}{\code{signature(object = "Regime")}: 
      Set the parameter estimates for all decision rules. Method not exported.}

    \item{regimeCoef}{\code{signature(object = "Regime_DecisionPointList")}: 
      Retrieve regime parameter estimates. }

  }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("Regime_DecisionPointList")

data <- data.frame(x1 = rnorm(10), x2 = rnorm(10))

regime <- list()
regime[[1L]] <- function(a,data){
                  return(data$x1 < a)
                }

regime[[2L]] <- function(a,data){
                  return(data$x2 < a)
                }

regObj <- list()
regObj[[1L]] <- new("Regime",
                    nVars = 1L,
                    vNames = c("a"),
                    func = regime[[1L]],
                    pars = c(0.1))
regObj[[2L]] <- new("Regime",
                    nVars = 1L,
                    vNames = c("a"),
                    func = regime[[2L]],
                    pars = c(0.1))

regObj <- new("DecisionPointList", loo = regObj)
regObj <- new("Regime_DecisionPointList", regObj)

is(regObj)

DynTxRegime:::.getNumPars(object = regObj)
DynTxRegime:::.getParNames(object = regObj)
DynTxRegime:::.getPars(object = regObj)
DynTxRegime:::.getRegimeFunction(object = regObj)
DynTxRegime:::.predictOptimalTx(x = regObj, newdata = data, dp=1)
DynTxRegime:::.predictOptimalTx(x = regObj, newdata = data, dp=2)
regObj <- DynTxRegime:::.setPars(object = regObj, pars = c(0.2,0.2))
DynTxRegime:::.getPars(object = regObj)
regimeCoef(object = regObj)
}

\keyword{internal}
