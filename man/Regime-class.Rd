\name{Regime-class}
\docType{class}
\alias{Regime-class}
\alias{regimeCoef,Regime-method}

\title{Class \code{"Regime"}}
\description{
All information regarding user defined regime function for a single decision point.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regime", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{nVars}:}{An object of class \code{"integer."} 
       The number of parameters in regime function. }

    \item{\code{vNames}:}{An object of class \code{"character."} 
       The names of parameters in regime function. }

    \item{\code{func}:}{An object of class \code{"function."} 
       The user provided function defining treatment regime. }

    \item{\code{pars}:}{An object of class \code{"numeric."} 
       The regime parameter estimates. }
  }
}

\section{Extends}{
Classes \code{"\link{RegimeObject-class}"} and
\code{"\link{SingleDecisionPoint-class}"} directly.
}

\section{Methods}{
  \describe{
    \item{.getNumPars}{\code{signature(object = "Regime")}: 
      Retrieve the number of parameters. Method not exported.}

    \item{.getParNames}{\code{signature(object = "Regime")}: 
      Retrieve the names of the parameters. Method not exported.}

    \item{.getPars}{\code{signature(object = "Regime")}: 
      Retrieve the parameter estimates. Method not exported.}

    \item{.getRegimeFunction}{\code{signature(object = "Regime")}: 
      Retrieve the decision rule function. Method not exported.}

    \item{.predictOptimalTx}{\code{signature(x = "Regime", newdata = "data.frame")}:
      Estimate the optimal treatment for newdata. Method not exported.}

    \item{.setPars}{\code{signature(object = "Regime")}: 
      Set the parameter estimates. Method not exported.}

    \item{regimeCoef}{\code{signature(object = "Regime")}: 
      Retrieve regime parameter estimates. }

  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("Regime")

data <- data.frame(x1 = rnorm(10), x2 = rnorm(10), x3 = rnorm(10))

regime <- function(a,b,c,data){
            return(a*data$x1 + b*data$x2 + c*data$x3 < 0)
          }

regObj <- new("Regime",
              nVars = 3L,
              vNames = c("a","b","c"),
              func = regime,
              pars = c(0.1,0.2,0.3))

is(regObj)

DynTxRegime:::.getNumPars(object = regObj)
DynTxRegime:::.getParNames(object = regObj)
DynTxRegime:::.getPars(object = regObj)
DynTxRegime:::.getRegimeFunction(object = regObj)
DynTxRegime:::.predictOptimalTx(x = regObj, newdata = data)
regObj <- DynTxRegime:::.setPars(object = regObj, pars = c(0.2,0.1,0.3))
DynTxRegime:::.getPars(object = regObj)
regimeCoef(object = regObj)
}

\keyword{internal}
